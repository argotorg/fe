use camino::Utf8PathBuf;
use driver::check_path;

pub fn check(path: &Utf8PathBuf, show_downstream: bool) {
    if check_path(path, show_downstream) {
        std::process::exit(1);
    }
}
