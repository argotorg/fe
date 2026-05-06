use common::InputDb;
use criterion::{Criterion, SamplingMode, criterion_group, criterion_main};
use driver::DriverDataBase;
use url::Url;

const SAMPLE_CODE: &str = r#"
struct Point {
    pub x: i32
    pub y: i32
}

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn distance(self, other: Point) -> i32 {
        let dx = self.x - other.x
        let dy = self.y - other.y
        dx * dx + dy * dy
    }

    pub fn translate(self, dx: i32, dy: i32) -> Point {
        Point::new(self.x + dx, self.y + dy)
    }
}

pub fn origin() -> Point {
    Point::new(0, 0)
}

pub fn midpoint(a: Point, b: Point) -> Point {
    Point::new((a.x + b.x) / 2, (a.y + b.y) / 2)
}
"#;

const EDITED_CODE: &str = r#"
struct Point {
    pub x: i32
    pub y: i32
}

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn distance(self, other: Point) -> i32 {
        let dx = self.x - other.x
        let dy = self.y - other.y
        dx * dx + dy * dy
    }

    pub fn translate(self, dx: i32, dy: i32) -> Point {
        Point::new(self.x + dx, self.y + dy)
    }

    pub fn scale(self, factor: i32) -> Point {
        Point::new(self.x * factor, self.y * factor)
    }
}

pub fn zero() -> Point {
    Point::new(0, 0)
}

pub fn midpoint(a: Point, b: Point) -> Point {
    Point::new((a.x + b.x) / 2, (a.y + b.y) / 2)
}
"#;

fn scip_regen(c: &mut Criterion) {
    let mut g = c.benchmark_group("scip_regen");
    g.sampling_mode(SamplingMode::Flat);
    g.sample_size(10);

    let temp = tempfile::tempdir().expect("create temp dir");
    let file_path = temp.path().join("main.fe");
    let file_url = Url::from_file_path(&file_path).unwrap();
    let ingot_url = Url::from_directory_path(temp.path()).unwrap();

    let mut db = DriverDataBase::default();
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(SAMPLE_CODE.to_string()));

    // Warm the salsa cache with initial analysis
    let ingot = db
        .workspace()
        .containing_ingot(&db, file_url.clone())
        .expect("ingot");
    db.run_on_ingot(ingot);

    // Warm the SCIP generation cache (first call is cold)
    let _ = semantic_indexing::scip_batch::generate_scip(&db, &ingot_url);

    // Benchmark: warm SCIP regen after edit (salsa-cached path)
    g.bench_function("warm_edit_scip", |b| {
        b.iter(|| {
            db.workspace()
                .update(&mut db, file_url.clone(), EDITED_CODE.to_string());
            let result = semantic_indexing::scip_batch::generate_scip(&db, &ingot_url);
            db.workspace()
                .update(&mut db, file_url.clone(), SAMPLE_CODE.to_string());
            result
        });
    });

    g.finish();
}

criterion_group!(benches, scip_regen);
criterion_main!(benches);
