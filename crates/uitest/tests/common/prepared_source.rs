use common::{InputDb, file::File};
use driver::{DriverDataBase, generation::prepare_file};
use url::Url;

pub fn diagnostics(
    db: &mut DriverDataBase,
    url: Url,
    content: &str,
    check: impl FnOnce(&DriverDataBase, File) -> String,
) -> String {
    let file = db
        .workspace()
        .touch(db, url.clone(), Some(content.to_string()));
    match prepare_file(db, file) {
        Ok(Some(prepared)) => {
            let file = prepared
                .workspace()
                .get(&prepared, &url)
                .expect("prepared view keeps the fixture");
            check(&prepared, file)
        }
        Ok(None) => check(db, file),
        Err(error) => format!(
            "{}..{}: {}",
            u32::from(error.range.start()),
            u32::from(error.range.end()),
            error.cause
        ),
    }
}
