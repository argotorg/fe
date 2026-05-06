//! Benchmark for the LSP doc reload hot path (issue #1424).
//!
//! Measures the cost of SCIP generation after file edits.
//! With salsa caching (Layers 1-3), builtins never recompute and only
//! the user ingot's changed modules trigger work.
//!
//! Run: cargo bench -p fe-language-server --bench doc_reload

use common::InputDb;
use criterion::{Criterion, SamplingMode, criterion_group, criterion_main};
use driver::DriverDataBase;
use url::Url;

fn rss_mb() -> f64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmRSS:"))
                .and_then(|l| l.split_whitespace().nth(1))
                .and_then(|v| v.parse::<f64>().ok())
        })
        .map(|kb| kb / 1024.0)
        .unwrap_or(0.0)
}

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

fn doc_reload(c: &mut Criterion) {
    let mut g = c.benchmark_group("doc_reload");
    g.sampling_mode(SamplingMode::Flat);
    g.sample_size(10);

    let temp = tempfile::tempdir().expect("create temp dir");
    let file_path = temp.path().join("main.fe");
    let file_url = Url::from_file_path(&file_path).unwrap();
    let ingot_url = Url::from_directory_path(temp.path()).unwrap();

    let mut db = DriverDataBase::default();
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(SAMPLE_CODE.to_string()));

    // Warm salsa cache (initial analysis + first SCIP gen)
    let ingot = db
        .workspace()
        .containing_ingot(&db, file_url.clone())
        .expect("ingot");
    db.run_on_ingot(ingot);
    let _ = semantic_indexing::scip_batch::generate_scip(&db, &ingot_url);

    // Measure: SCIP regen after single edit (salsa-cached, builtins skip)
    g.bench_function("scip_after_single_edit", |b| {
        b.iter(|| {
            db.workspace()
                .update(&mut db, file_url.clone(), EDITED_CODE.to_string());
            let result = semantic_indexing::scip_batch::generate_scip(&db, &ingot_url);
            db.workspace()
                .update(&mut db, file_url.clone(), SAMPLE_CODE.to_string());
            result
        });
    });

    // Measure: 20 rapid edits (the scenario from issue #1424)
    let rss_before = rss_mb();
    g.bench_function("20_rapid_edits", |b| {
        let edits: Vec<String> = (0..20)
            .map(|i| SAMPLE_CODE.replace("origin", &format!("origin_{i}")))
            .collect();

        b.iter(|| {
            for edit in &edits {
                db.workspace()
                    .update(&mut db, file_url.clone(), edit.clone());
                let _ = semantic_indexing::scip_batch::generate_scip(&db, &ingot_url);
            }
            db.workspace()
                .update(&mut db, file_url.clone(), SAMPLE_CODE.to_string());
        });
    });
    let rss_after = rss_mb();
    eprintln!(
        "[doc_reload] RSS: before={rss_before:.1}MB after={rss_after:.1}MB delta={:.1}MB",
        rss_after - rss_before
    );

    g.finish();
}

criterion_group!(benches, doc_reload);
criterion_main!(benches);
