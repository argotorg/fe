use std::{
    collections::hash_map::DefaultHasher,
    fs,
    hash::{Hash, Hasher},
    path::Path,
};

fn hash_tree(path: &Path, hash: &mut DefaultHasher) {
    path.hash(hash);
    if path.is_dir() {
        let mut entries = fs::read_dir(path)
            .expect("read builtin library directory")
            .map(|entry| entry.expect("read builtin library entry").path())
            .collect::<Vec<_>>();
        entries.sort();
        for entry in entries {
            hash_tree(&entry, hash);
        }
    } else {
        fs::read(path)
            .expect("read builtin library source")
            .hash(hash);
    }
}

fn main() {
    let mut hash = DefaultHasher::new();
    for directory in ["../../ingots/core", "../../ingots/std"] {
        // Include directory membership: a newly added embedded file is absent
        // from the previous expansion's include_bytes dependency list.
        println!("cargo:rerun-if-changed={directory}");
        hash_tree(Path::new(directory), &mut hash);
    }
    // Make source membership/content an explicit rustc environment dependency
    // as well, so compiler caches cannot reuse an older macro expansion.
    println!(
        "cargo:rustc-env=FE_BUILTIN_FINGERPRINT={:016x}",
        hash.finish()
    );
}
