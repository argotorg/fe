//! Immutable source-package composition for generated artifacts.
//!
//! Packages retain source recipes, while only the newest composed root retains
//! its checked database. Composition exposes ordinary Fe package dependencies:
//! public surface and lexical shadowing keep their normal language behavior.

use std::{
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use url::Url;

use super::{FrozenArtifact, GeneratedFunction, GenerationProvenance, diagnostics};
use crate::DriverDataBase;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackageLimits {
    max_packages: usize,
    max_source_bytes: usize,
}

impl PackageLimits {
    pub fn new(max_packages: usize, max_source_bytes: usize) -> Self {
        Self {
            max_packages,
            max_source_bytes,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PackageErrorKind {
    Admission,
    Output,
    Limit,
}

#[derive(Debug)]
pub struct PackageError {
    pub kind: PackageErrorKind,
    pub message: String,
    pub materializations: Option<Vec<MaterializedPackage>>,
}

impl PackageError {
    fn new(kind: PackageErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            materializations: None,
        }
    }

    fn with_materializations(mut self, materializations: Vec<MaterializedPackage>) -> Self {
        self.materializations = Some(materializations);
        self
    }
}

impl fmt::Display for PackageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for PackageError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MaterializedPackage {
    pub source_url: Url,
    pub source: String,
    pub dependencies: Vec<(String, Url)>,
    pub generation: Option<GenerationProvenance>,
}

#[derive(Clone)]
enum RecipeHandle {
    Generated(Rc<GeneratedFunction>),
    Composed(Rc<Recipe>),
}

impl RecipeHandle {
    fn source(&self) -> &str {
        match self {
            Self::Generated(artifact) => artifact.source(),
            Self::Composed(recipe) => &recipe.source,
        }
    }

    fn dependencies(&self) -> &[(String, RecipeHandle)] {
        match self {
            Self::Generated(_) => &[],
            Self::Composed(recipe) => &recipe.dependencies,
        }
    }

    fn generation(&self) -> Option<&GenerationProvenance> {
        match self {
            Self::Generated(artifact) => Some(artifact.provenance()),
            Self::Composed(_) => None,
        }
    }

    fn key(&self) -> RecipeKey {
        match self {
            Self::Generated(artifact) => RecipeKey::Generated(Rc::as_ptr(artifact)),
            Self::Composed(recipe) => RecipeKey::Composed(Rc::as_ptr(recipe)),
        }
    }
}

struct Recipe {
    source: String,
    dependencies: Vec<(String, RecipeHandle)>,
}

struct CheckedRoot {
    db: DriverDataBase,
    file: File,
    materializations: Vec<MaterializedPackage>,
}

/// An immutable package recipe with an optional checked root realization.
/// Cloning preserves recipe identity. Composing a package retains only child
/// recipes, avoiding retention of every prior composed database in a chain.
#[derive(Clone)]
pub struct FrozenPackage {
    node: RecipeHandle,
    checked: Option<Rc<CheckedRoot>>,
}

impl From<FrozenArtifact> for FrozenPackage {
    fn from(artifact: FrozenArtifact) -> Self {
        Self {
            node: RecipeHandle::Generated(artifact.artifact),
            checked: None,
        }
    }
}

impl FrozenPackage {
    pub fn database(&self) -> &DriverDataBase {
        match &self.checked {
            Some(checked) => &checked.db,
            None => match &self.node {
                RecipeHandle::Generated(artifact) => artifact.database(),
                RecipeHandle::Composed(_) => {
                    unreachable!("a composed package always owns its checked root")
                }
            },
        }
    }

    pub fn file(&self) -> File {
        match &self.checked {
            Some(checked) => checked.file,
            None => match &self.node {
                RecipeHandle::Generated(artifact) => artifact.file(),
                RecipeHandle::Composed(_) => {
                    unreachable!("a composed package always owns its checked root")
                }
            },
        }
    }

    pub fn source(&self) -> &str {
        self.node.source()
    }

    pub fn materializations(&self) -> Option<&[MaterializedPackage]> {
        self.checked
            .as_ref()
            .map(|checked| checked.materializations.as_slice())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum RecipeKey {
    Generated(*const GeneratedFunction),
    Composed(*const Recipe),
}

struct Plan<'a> {
    nodes: Vec<&'a RecipeHandle>,
    ids: HashMap<RecipeKey, usize>,
    check_order: Vec<usize>,
    root_dependencies: Vec<(&'a str, usize)>,
}

struct Frame<'a> {
    node: &'a RecipeHandle,
    next_dependency: usize,
}

/// Compose a checked package from source and immutable package dependencies.
/// Limits count the new root, each unique transitive recipe node, all unique
/// node source bytes, and every dependency-edge alias byte. They do not model
/// allocations performed by ordinary compiler checking.
pub fn compose_packages(
    source: String,
    imports: &[(&str, &FrozenPackage)],
    limits: PackageLimits,
) -> Result<FrozenPackage, PackageError> {
    let plan = plan(&source, imports, limits)?;
    let root_id = plan.nodes.len();
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();

    let mut files = Vec::with_capacity(plan.nodes.len());
    let mut materializations = Vec::with_capacity(plan.nodes.len() + 1);
    for (id, node) in plan.nodes.iter().enumerate() {
        let dependencies = dependency_receipts(node.dependencies(), &plan.ids);
        let config = package_config(id, node.dependencies(), &plan.ids);
        touch(&mut db, &config_url(id), config);
        let source_url = parsed_url(&source_url(id));
        let file =
            db.workspace()
                .touch(&mut db, source_url.clone(), Some(node.source().to_owned()));
        files.push(file);
        materializations.push(MaterializedPackage {
            source_url,
            source: node.source().to_owned(),
            dependencies,
            generation: node.generation().cloned(),
        });
    }

    let root_dependencies = plan
        .root_dependencies
        .iter()
        .map(|(alias, id)| ((*alias).to_owned(), parsed_url(&source_url(*id))))
        .collect::<Vec<_>>();
    let root_config = root_config(root_id, &plan.root_dependencies);
    touch(&mut db, &config_url(root_id), root_config);
    let root_source_url = parsed_url(&source_url(root_id));
    let root_file = db
        .workspace()
        .touch(&mut db, root_source_url.clone(), Some(source.clone()));
    materializations.push(MaterializedPackage {
        source_url: root_source_url,
        source: source.clone(),
        dependencies: root_dependencies,
        generation: None,
    });

    for id in plan.check_order {
        if let Some(message) = diagnostics(&db, files[id]) {
            return Err(PackageError::new(PackageErrorKind::Output, message)
                .with_materializations(materializations));
        }
    }
    if let Some(message) = diagnostics(&db, root_file) {
        return Err(PackageError::new(PackageErrorKind::Output, message)
            .with_materializations(materializations));
    }

    let dependencies = imports
        .iter()
        .map(|(alias, package)| ((*alias).to_owned(), package.node.clone()))
        .collect();
    Ok(FrozenPackage {
        node: RecipeHandle::Composed(Rc::new(Recipe {
            source,
            dependencies,
        })),
        checked: Some(Rc::new(CheckedRoot {
            db,
            file: root_file,
            materializations,
        })),
    })
}

fn plan<'a>(
    source: &str,
    imports: &[(&'a str, &'a FrozenPackage)],
    limits: PackageLimits,
) -> Result<Plan<'a>, PackageError> {
    if limits.max_packages == 0 {
        return Err(package_limit());
    }
    let mut bytes = source.len();
    if bytes > limits.max_source_bytes {
        return Err(source_limit());
    }
    for (alias, _) in imports {
        bytes = add_bytes(bytes, alias.len(), limits.max_source_bytes)?;
    }
    validate_aliases(imports.iter().map(|(alias, _)| *alias))?;

    let mut nodes = Vec::new();
    let mut ids = HashMap::new();
    let mut states = HashMap::<RecipeKey, bool>::new();
    let mut check_order = Vec::new();
    for (_, package) in imports {
        let root = &package.node;
        match states.get(&root.key()) {
            Some(true) => continue,
            Some(false) => unreachable!("a prior root traversal cannot remain unfinished"),
            None => admit_node(root, limits, &mut bytes, &mut nodes, &mut ids, &mut states)?,
        }
        let mut stack = vec![Frame {
            node: root,
            next_dependency: 0,
        }];
        while let Some(frame) = stack.last_mut() {
            if let Some((_, dependency)) = frame.node.dependencies().get(frame.next_dependency) {
                frame.next_dependency += 1;
                match states.get(&dependency.key()) {
                    Some(true) => {}
                    Some(false) => {
                        return Err(PackageError::new(
                            PackageErrorKind::Admission,
                            "frozen package recipe graph contains a cycle",
                        ));
                    }
                    None => {
                        admit_node(
                            dependency,
                            limits,
                            &mut bytes,
                            &mut nodes,
                            &mut ids,
                            &mut states,
                        )?;
                        stack.push(Frame {
                            node: dependency,
                            next_dependency: 0,
                        });
                    }
                }
            } else {
                let completed = stack.pop().expect("active DFS frame").node;
                states.insert(completed.key(), true);
                check_order.push(ids[&completed.key()]);
            }
        }
    }

    let root_dependencies = imports
        .iter()
        .map(|(alias, package)| (*alias, ids[&package.node.key()]))
        .collect();
    Ok(Plan {
        nodes,
        ids,
        check_order,
        root_dependencies,
    })
}

fn admit_node<'a>(
    node: &'a RecipeHandle,
    limits: PackageLimits,
    bytes: &mut usize,
    nodes: &mut Vec<&'a RecipeHandle>,
    ids: &mut HashMap<RecipeKey, usize>,
    states: &mut HashMap<RecipeKey, bool>,
) -> Result<(), PackageError> {
    let package_count = nodes.len().checked_add(2).ok_or_else(package_limit)?;
    if package_count > limits.max_packages {
        return Err(package_limit());
    }
    *bytes = add_bytes(*bytes, node.source().len(), limits.max_source_bytes)?;
    for (alias, _) in node.dependencies() {
        *bytes = add_bytes(*bytes, alias.len(), limits.max_source_bytes)?;
    }
    validate_aliases(node.dependencies().iter().map(|(alias, _)| alias.as_str()))?;

    let key = node.key();
    let id = nodes.len();
    nodes.push(node);
    ids.insert(key, id);
    states.insert(key, false);
    Ok(())
}

fn valid_alias(alias: &str) -> bool {
    let mut chars = alias.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii()
        && (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch.is_ascii() && (ch == '_' || ch.is_ascii_alphanumeric()))
        && !matches!(alias, "core" | "std" | "ingot" | "self" | "super")
}

fn validate_aliases<'a>(aliases: impl Iterator<Item = &'a str>) -> Result<(), PackageError> {
    let mut seen = HashSet::new();
    for alias in aliases {
        if !valid_alias(alias) {
            return Err(PackageError::new(
                PackageErrorKind::Admission,
                format!("invalid frozen package alias `{alias}`"),
            ));
        }
        if !seen.insert(alias) {
            return Err(PackageError::new(
                PackageErrorKind::Admission,
                format!("duplicate frozen package alias `{alias}`"),
            ));
        }
    }
    Ok(())
}

fn add_bytes(total: usize, added: usize, limit: usize) -> Result<usize, PackageError> {
    total
        .checked_add(added)
        .filter(|total| *total <= limit)
        .ok_or_else(source_limit)
}

fn package_limit() -> PackageError {
    PackageError::new(
        PackageErrorKind::Limit,
        "frozen package count limit exceeded",
    )
}

fn source_limit() -> PackageError {
    PackageError::new(
        PackageErrorKind::Limit,
        "frozen package source byte limit exceeded",
    )
}

fn config_url(id: usize) -> String {
    format!("file:///frozen-packages/package_{id}/fe.toml")
}

fn source_url(id: usize) -> String {
    format!("file:///frozen-packages/package_{id}/src/lib.fe")
}

fn parsed_url(url: &str) -> Url {
    Url::parse(url).expect("fixed frozen package URL is valid")
}

fn touch(db: &mut DriverDataBase, url: &str, source: String) -> File {
    db.workspace().touch(db, parsed_url(url), Some(source))
}

fn package_config(
    id: usize,
    dependencies: &[(String, RecipeHandle)],
    ids: &HashMap<RecipeKey, usize>,
) -> String {
    let mapped = dependencies
        .iter()
        .map(|(alias, dependency)| (alias.as_str(), ids[&dependency.key()]))
        .collect::<Vec<_>>();
    root_config(id, &mapped)
}

fn root_config(id: usize, dependencies: &[(&str, usize)]) -> String {
    let mut config = format!("[ingot]\nname = \"frozen_package_{id}\"\nversion = \"0.0.0\"\n");
    if !dependencies.is_empty() {
        config.push_str("\n[dependencies]\n");
        for (alias, dependency) in dependencies {
            config.push_str(&format!(
                "{alias} = {{ path = \"../package_{dependency}\" }}\n"
            ));
        }
    }
    config
}

fn dependency_receipts(
    dependencies: &[(String, RecipeHandle)],
    ids: &HashMap<RecipeKey, usize>,
) -> Vec<(String, Url)> {
    dependencies
        .iter()
        .map(|(alias, dependency)| {
            (
                alias.clone(),
                parsed_url(&source_url(ids[&dependency.key()])),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composing_retains_recipes_without_retaining_child_checked_databases() {
        let child = compose_packages(
            "pub const fn value() -> bool { true }".into(),
            &[],
            PackageLimits::new(2, 4096),
        )
        .unwrap();
        let checked = Rc::downgrade(child.checked.as_ref().unwrap());
        let parent = compose_packages(
            "pub const fn value() -> bool { child::value() }".into(),
            &[("child", &child)],
            PackageLimits::new(2, 4096),
        )
        .unwrap();
        drop(child);
        assert!(checked.upgrade().is_none());
        assert_eq!(parent.materializations().unwrap().len(), 2);
    }
}
