use camino::Utf8PathBuf;
use common::{
    file::{File, IngotFileKind},
    indexmap::IndexMap,
    ingot::Ingot,
};
use cranelift_entity::{EntityRef, PrimaryMap, entity_impl};
use salsa::Update;

use super::{IdentId, TopLevelMod};
use crate::{HirDb, lower::map_file_to_mod_impl};

/// Error when a module is not found in a module tree.
/// This typically indicates either a cross-ingot query or a bug in incremental computation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleNotInTree;

/// This tree represents the structure of an ingot.
/// Internal modules are not included in this tree, instead, they are included
/// in [ScopeGraph](crate::hir_def::scope_graph::ScopeGraph).
///
/// This is used in later name resolution phase.
/// The tree is file contents agnostic, i.e., **only** depends on project
/// structure and crate dependency.
///
///
/// Example:
/// ```text
/// ingot/
/// ├─ main.fe
/// ├─ mod1.fe
/// ├─ mod1/
/// │  ├─ foo.fe
/// ├─ mod2.fe
/// ├─ mod2
/// │  ├─ bar.fe
/// ├─ mod3
/// │  ├─ baz.fe
/// ```
///
/// The resulting tree would be like below.
///
/// ```text
///           +------+
///     *---- | main |----*
///     |     +------+    |         +------+
///     |                 |         | baz  |
///     |                 |         +------+
///     v                 v
///  +------+          +------+
///  | mod2 |          | mod1 |
///  +------+          +------+
///     |                 |
///     |                 |
///     v                 v
///  +------+          +------+
///  | bar  |          | foo  |
///  +------+          +------+
///  ```
///
/// **NOTE:** `mod3` is not included in the main tree because it doesn't have a corresponding file.
/// As a result, `baz` is represented as a "floating" node.
/// In this case, the tree is actually a forest. But we don't need to care about it.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct ModuleTree<'db> {
    pub(crate) root: ModuleTreeNodeId,
    pub(crate) module_tree: PMap<ModuleTreeNodeId, ModuleTreeNode<'db>>,
    pub(crate) mod_map: IndexMap<TopLevelMod<'db>, ModuleTreeNodeId>,

    pub ingot: Ingot<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PMap<K: EntityRef, V>(PrimaryMap<K, V>);

unsafe impl<K, V> Update for PMap<K, V>
where
    K: EntityRef,
    V: Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_vec: Self) -> bool {
        unsafe {
            let old_vec: &mut PMap<K, V> = &mut *old_pointer;

            if old_vec.0.len() != new_vec.0.len() {
                return true;
            }

            let mut changed = false;
            for (old_element, new_element) in old_vec
                .0
                .values_mut()
                .zip(new_vec.0.into_iter().map(|(_, v)| v))
            {
                changed |= V::maybe_update(old_element, new_element);
            }

            changed
        }
    }
}

impl ModuleTree<'_> {
    /// Returns the tree node data of the given id.
    pub fn node_data(&self, id: ModuleTreeNodeId) -> &ModuleTreeNode<'_> {
        &self.module_tree.0[id]
    }

    /// Returns the tree node id of the given top level module.
    ///
    /// Returns `Err(ModuleNotInTree)` if the module is not in this tree,
    /// which indicates either a cross-ingot query or a bug in incremental computation.
    pub fn tree_node(&self, top_mod: TopLevelMod) -> Result<ModuleTreeNodeId, ModuleNotInTree> {
        self.mod_map.get(&top_mod).copied().ok_or_else(|| {
            tracing::error!(
                "TopLevelMod not found in module tree. Tree ingot: {:?}. \
                 This may indicate a cross-ingot query or incremental computation bug.",
                self.ingot,
            );
            ModuleNotInTree
        })
    }

    /// Returns the tree node data of the given top level module.
    ///
    /// Returns `Err(ModuleNotInTree)` if the module is not in this tree.
    pub fn tree_node_data(
        &self,
        top_mod: TopLevelMod,
    ) -> Result<&ModuleTreeNode<'_>, ModuleNotInTree> {
        self.tree_node(top_mod).map(|id| &self.module_tree.0[id])
    }

    /// Returns the root of the tree, which corresponds to the ingot root file.
    pub fn root(&self) -> ModuleTreeNodeId {
        self.root
    }

    pub fn root_data(&self) -> &ModuleTreeNode<'_> {
        self.node_data(self.root)
    }

    /// Returns an iterator of all top level modules in this ingot.
    pub fn all_modules(&self) -> impl Iterator<Item = TopLevelMod<'_>> + '_ {
        self.mod_map.keys().copied()
    }

    /// Returns the parent of the given module, or `None` if it's a root or not found.
    /// Logs an error if the module is not in this tree.
    pub fn parent(&self, top_mod: TopLevelMod) -> Option<TopLevelMod<'_>> {
        let node = self.tree_node_data(top_mod).ok()?;
        node.parent.map(|id| self.module_tree.0[id].top_mod)
    }

    /// Returns the children of the given module.
    /// Returns an empty iterator if the module is not in this tree (error is logged).
    pub fn children(&self, top_mod: TopLevelMod) -> impl Iterator<Item = TopLevelMod<'_>> + '_ {
        self.tree_node_data(top_mod)
            .ok()
            .into_iter()
            .flat_map(|node| node.children.iter())
            .map(move |&id| {
                let node = &self.module_tree.0[id];
                node.top_mod
            })
    }
}

/// Returns a module tree of the given ingot. The resulted tree only includes
/// top level modules. This function only depends on an ingot structure and
/// external ingot dependency, and not depends on file contents.
#[salsa::tracked(return_ref)]
pub(crate) fn module_tree_impl<'db>(db: &'db dyn HirDb, ingot: Ingot<'db>) -> ModuleTree<'db> {
    // Build everything in one tracked function to avoid passing complex data between functions
    let mut module_tree = PrimaryMap::default();
    let mut mod_map = IndexMap::default();
    let mut path_map = IndexMap::default();

    // Collect source modules
    let files = ingot.files(db);
    for (_, file) in files.iter() {
        if let Some(IngotFileKind::Source) = file.kind(db) {
            let top_mod = map_file_to_mod_impl(db, file);
            let module_id = module_tree.push(ModuleTreeNode::new(top_mod));
            let path = file.path(db).as_ref().expect("couldn't get path").clone();
            path_map.insert(path, module_id);
            mod_map.insert(top_mod, module_id);
        }
    }

    // Find root - if there's no root file, use the first source file as fallback
    let root_file = match ingot.root_file(db) {
        Ok(file) => file,
        Err(_) => {
            // No root file found - use first source file as fallback root
            // This handles non-conformant ingots (e.g., directories without src/lib.fe)
            let first_source = files.iter().find_map(|(_, file)| {
                if matches!(file.kind(db), Some(IngotFileKind::Source)) {
                    Some(file)
                } else {
                    None
                }
            });
            match first_source {
                Some(file) => file,
                None => {
                    // No source files at all - this is a bug, ingots should have source files
                    panic!(
                        "Ingot {:?} has no source files. This indicates a bug in ingot construction.",
                        ingot
                    );
                }
            }
        }
    };
    let root_mod = map_file_to_mod_impl(db, root_file);
    let root = mod_map[&root_mod];

    // Build parent-child relationships
    for (_, child_file) in files.iter() {
        if child_file == root_file {
            continue;
        }

        if let Some(IngotFileKind::Source) = child_file.kind(db)
            && let Some(parent_id) = find_parent_module(db, child_file, root_file, &path_map)
        {
            let child_mod = map_file_to_mod_impl(db, child_file);
            let child_id = mod_map[&child_mod];

            module_tree[parent_id].children.push(child_id);
            module_tree[child_id].parent = Some(parent_id);
        }
    }

    ModuleTree {
        root,
        module_tree: PMap(module_tree),
        mod_map,
        ingot,
    }
}

/// Find the parent module for a given file
fn find_parent_module<'db>(
    db: &'db dyn HirDb,
    child_file: File,
    root_file: File,
    path_map: &'db IndexMap<Utf8PathBuf, ModuleTreeNodeId>,
) -> Option<ModuleTreeNodeId> {
    let root_path = root_file.path(db).as_ref()?;
    let child_path = child_file.path(db).as_ref()?;

    // If in same directory as root, parent is root
    if child_path.parent() == root_path.parent() {
        return path_map.get(root_path).copied();
    }

    // Otherwise, find parent based on directory structure
    let file_dir = child_path.parent()?;
    let parent_dir = file_dir.parent()?;
    let parent_mod_stem = file_dir.file_name()?;
    let parent_mod_path = parent_dir.join(parent_mod_stem).with_extension("fe");

    path_map.get(&parent_mod_path).copied()
}

/// A top level module that is one-to-one mapped to a file.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ModuleTreeNode<'db> {
    pub top_mod: TopLevelMod<'db>,
    /// A parent of the top level module.
    /// This is `None` if
    /// 1. the module is a root module or
    /// 2. the module is a "floating" module.
    pub parent: Option<ModuleTreeNodeId>,
    /// A list of child top level module.
    pub children: Vec<ModuleTreeNodeId>,
}

impl<'db> ModuleTreeNode<'db> {
    fn new(top_mod: TopLevelMod<'db>) -> Self {
        Self {
            top_mod,
            parent: None,
            children: Vec::new(),
        }
    }
    pub fn name(&self, db: &'db dyn HirDb) -> IdentId<'db> {
        self.top_mod.name(db)
    }
}

/// An opaque identifier for a module tree node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ModuleTreeNodeId(u32);
entity_impl!(ModuleTreeNodeId);

#[cfg(test)]
mod tests {

    use common::{InputDb, ingot::IngotBaseUrl};
    use url::Url;

    use crate::{core::hir_def::HirIngot, lower, test_db::TestDb};

    /// Test that querying a module from one ingot in another ingot's tree
    /// doesn't panic - it should return None/empty gracefully.
    ///
    /// This reproduces a bug where the LSP would panic during diagnostics
    /// when a TopLevelMod was looked up in a ModuleTree that didn't contain it.
    #[test]
    fn module_tree_cross_ingot_no_panic() {
        let mut db = TestDb::default();

        // Create two separate ingots - use fe.toml URLs so directory() works correctly
        // (directory() pops the last path segment, so file:///a/b/fe.toml -> file:///a/b/)
        let ingot1_base = Url::parse("file:///workspace/ingot1/fe.toml").unwrap();
        let ingot2_base = Url::parse("file:///workspace/ingot2/fe.toml").unwrap();

        // Touch the fe.toml files to create the ingots
        ingot1_base.touch(&mut db, "fe.toml".into(), None);
        ingot2_base.touch(&mut db, "fe.toml".into(), None);

        // Create source files in each ingot
        let file1 = IngotBaseUrl::touch(&ingot1_base, &mut db, "src/lib.fe".into(), None);
        let file2 = IngotBaseUrl::touch(&ingot2_base, &mut db, "src/lib.fe".into(), None);

        // The files should be different (different URLs)
        assert_ne!(
            file1, file2,
            "files from different ingots should be distinct"
        );

        // Get modules from both ingots
        let mod1 = lower::map_file_to_mod(&db, file1);
        let mod2 = lower::map_file_to_mod(&db, file2);

        // The modules should be different (based on different files)
        assert_ne!(
            mod1, mod2,
            "modules from different ingots should be distinct"
        );

        // Get ingot2's module tree
        let ingot2 = db
            .workspace()
            .containing_ingot(&db, ingot2_base)
            .expect("ingot2 should exist");
        let tree2 = ingot2.module_tree(&db);

        // tree2 should contain mod2 but NOT mod1
        assert!(
            tree2.tree_node(mod2).is_ok(),
            "tree2 should contain its own module"
        );
        assert!(
            tree2.tree_node(mod1).is_err(),
            "tree2 should NOT contain mod1 from ingot1"
        );

        // Querying mod1's children/parent from tree2 should return empty/None (error is logged)
        assert_eq!(tree2.children(mod1).count(), 0);
        assert!(tree2.parent(mod1).is_none());
    }

    #[test]
    fn module_tree() {
        let mut db = TestDb::default();

        let index = db.workspace();
        let ingot_base = Url::parse("file:///foo/fargo").unwrap();

        // fe.toml anchors the ingot base
        ingot_base.touch(&mut db, "fe.toml".into(), None);

        let local_root = IngotBaseUrl::touch(&ingot_base, &mut db, "src/lib.fe".into(), None);
        let mod1 = IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod1.fe".into(), None);
        let mod2 = IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod2.fe".into(), None);
        let foo = IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod1/foo.fe".into(), None);
        let bar = IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod2/bar.fe".into(), None);
        let baz = IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod2/baz.fe".into(), None);
        let _floating =
            IngotBaseUrl::touch(&ingot_base, &mut db, "src/mod3/floating.fe".into(), None);

        let local_root_mod = lower::map_file_to_mod(&db, local_root);
        let mod1_mod = lower::map_file_to_mod(&db, mod1);
        let mod2_mod = lower::map_file_to_mod(&db, mod2);
        let foo_mod = lower::map_file_to_mod(&db, foo);
        let bar_mod = lower::map_file_to_mod(&db, bar);
        let baz_mod = lower::map_file_to_mod(&db, baz);

        let local_tree = lower::module_tree(
            &db,
            index
                .containing_ingot(&db, ingot_base)
                .expect("Failed to construct ingot"),
        );
        let root_node = local_tree.root_data();
        assert_eq!(root_node.top_mod, local_root_mod);
        assert_eq!(root_node.children.len(), 2);

        for &child in &root_node.children {
            if child == local_tree.tree_node(mod1_mod).unwrap() {
                let child = local_tree.node_data(child);
                assert_eq!(child.parent, Some(local_tree.root()));
                assert_eq!(child.children.len(), 1);
                assert_eq!(child.children[0], local_tree.tree_node(foo_mod).unwrap());
            } else if child == local_tree.tree_node(mod2_mod).unwrap() {
                let child = local_tree.node_data(child);
                assert_eq!(child.parent, Some(local_tree.root()));
                assert_eq!(child.children.len(), 2);
                assert_eq!(child.children[0], local_tree.tree_node(bar_mod).unwrap());
                assert_eq!(child.children[1], local_tree.tree_node(baz_mod).unwrap());
            } else {
                panic!("unexpected child")
            }
        }
    }
}
