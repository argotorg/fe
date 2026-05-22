use common::InputDb;
use common::fact_consumer::{Fact, FactConsumer};
use common::hash_consumer::{DimHashes, HashConsumer};
use common::ir_describe::{DescribeCtx, IrDescribe};
use driver::DriverDataBase;
use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use url::Url;

pub struct FunctionRecord {
    pub symbol: String,
    pub hashes: DimHashes,
    pub stmt_count: usize,
    pub origin_count: usize,
}

pub struct SourceAnalysis {
    pub db: DriverDataBase,
    pub file_url: Url,
    pub functions: Vec<FunctionRecord>,
    pub facts: Vec<Fact>,
}

impl SourceAnalysis {
    pub fn from_source(source: &str) -> Result<Self, String> {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::from_file_path(std::env::temp_dir().join("analyze_input.fe")).expect("temp path");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .ok_or("failed to resolve file")?;
        let top_mod = db.top_mod(file);
        let package = mir::build_runtime_package(&db, top_mod).map_err(|e| format!("{e}"))?;

        let cx = DescribeCtx::new(&db);
        let mut functions = Vec::new();
        let mut all_facts = Vec::new();
        let mut next_id: u32 = 0;

        for func in package.functions(&db) {
            let body = func.instance(&db).body(&db);
            let symbol = func.symbol(&db);

            let mut composite = (HashConsumer::new(), FactConsumer::with_starting_id(next_id));
            body.describe(&cx, &mut composite);
            let hashes = composite.0.into_result().unwrap_or(DimHashes {
                values: [0; common::ir_describe::Dim::COUNT],
            });
            next_id = composite.1.next_id();

            let mut stmt_count = 0;
            let mut origin_count = 0;
            for block in &body.blocks {
                stmt_count += block.stmts.len();
                origin_count += block.stmt_origins.len();
            }

            all_facts.extend(composite.1.into_facts());
            functions.push(FunctionRecord {
                symbol,
                hashes,
                stmt_count,
                origin_count,
            });
        }

        Ok(Self {
            db,
            file_url,
            functions,
            facts: all_facts,
        })
    }

    pub fn function_hashes(&self) -> Vec<(&str, &DimHashes)> {
        self.functions
            .iter()
            .map(|f| (f.symbol.as_str(), &f.hashes))
            .collect()
    }

    pub fn find_hash(&self, substr: &str) -> Option<&DimHashes> {
        self.functions
            .iter()
            .find(|f| f.symbol.contains(substr))
            .map(|f| &f.hashes)
    }

    pub fn package(&self) -> mir::RuntimePackage<'_> {
        let file = self
            .db
            .workspace()
            .get(&self.db, &self.file_url)
            .expect("file");
        let top_mod = self.db.top_mod(file);
        mir::build_runtime_package(&self.db, top_mod).expect("compile")
    }

    pub fn describe_ctx(&self) -> DescribeCtx<'_> {
        DescribeCtx::new(&self.db)
    }
    pub fn category_breakdown(&self) -> Vec<CategoryEntry> {
        let mut cats: HashMap<&'static str, (usize, usize)> = HashMap::new();
        for f in &self.functions {
            let cat = categorize_function(&f.symbol);
            let entry = cats.entry(cat).or_default();
            entry.0 += 1;
            entry.1 += f.stmt_count;
        }
        let total_stmts: usize = self.functions.iter().map(|f| f.stmt_count).sum();
        let mut result: Vec<_> = cats
            .into_iter()
            .map(|(name, (count, stmts))| {
                let pct = if total_stmts > 0 {
                    stmts as f64 / total_stmts as f64 * 100.0
                } else {
                    0.0
                };
                CategoryEntry {
                    name,
                    count,
                    stmts,
                    pct,
                }
            })
            .collect();
        result.sort_by_key(|a| Reverse(a.stmts));
        result
    }

    pub fn dedup_report(&self) -> DedupReport {
        let mut by_hash: HashMap<u128, Vec<&FunctionRecord>> = HashMap::new();
        for f in &self.functions {
            by_hash.entry(f.hashes.structure()).or_default().push(f);
        }

        let total_stmts: usize = self.functions.iter().map(|f| f.stmt_count).sum();
        let mut entries: Vec<DedupEntry> = Vec::new();

        for group in by_hash.values() {
            if group.len() < 2 {
                continue;
            }
            let copies = group.len();
            let stmts_per_copy = group[0].stmt_count;
            let wasted = stmts_per_copy * (copies - 1);
            let representative = shortest_symbol(group);
            entries.push(DedupEntry {
                representative,
                copies,
                stmts_per_copy,
                wasted,
            });
        }
        entries.sort_by_key(|a| Reverse(a.wasted));

        let total_wasted: usize = entries.iter().map(|e| e.wasted).sum();
        let pct_wasted = if total_stmts > 0 {
            total_wasted as f64 / total_stmts as f64 * 100.0
        } else {
            0.0
        };

        DedupReport {
            entries,
            total_wasted,
            pct_wasted,
        }
    }

    pub fn effect_summary(&self) -> Vec<EffectEntry> {
        let mut counts: HashMap<String, usize> = HashMap::new();
        for fact in &self.facts {
            if let Fact::NodeEffect { effect, .. } = fact {
                *counts.entry(effect.clone()).or_default() += 1;
            }
        }
        let mut result: Vec<_> = counts
            .into_iter()
            .map(|(effect, count)| EffectEntry { effect, count })
            .collect();
        result.sort_by_key(|a| Reverse(a.count));
        result
    }

    pub fn overview(&self) -> Overview {
        let total = self.functions.len();
        let mut unique: HashSet<u128> = HashSet::new();
        for f in &self.functions {
            unique.insert(f.hashes.structure());
        }
        let unique_structures = unique.len();
        let total_stmts: usize = self.functions.iter().map(|f| f.stmt_count).sum();
        let total_origins: usize = self.functions.iter().map(|f| f.origin_count).sum();
        let origin_pct = if total_stmts > 0 {
            total_origins as f64 / total_stmts as f64 * 100.0
        } else {
            0.0
        };
        let dup_pct = if total > 0 {
            (1.0 - unique_structures as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        Overview {
            total_functions: total,
            unique_structures,
            dup_pct,
            total_stmts,
            origin_coverage_pct: origin_pct,
        }
    }
}

pub struct CategoryEntry {
    pub name: &'static str,
    pub count: usize,
    pub stmts: usize,
    pub pct: f64,
}

pub struct DedupEntry {
    pub representative: String,
    pub copies: usize,
    pub stmts_per_copy: usize,
    pub wasted: usize,
}

pub struct DedupReport {
    pub entries: Vec<DedupEntry>,
    pub total_wasted: usize,
    pub pct_wasted: f64,
}

pub struct EffectEntry {
    pub effect: String,
    pub count: usize,
}

pub struct Overview {
    pub total_functions: usize,
    pub unique_structures: usize,
    pub dup_pct: f64,
    pub total_stmts: usize,
    pub origin_coverage_pct: f64,
}

fn categorize_function(symbol: &str) -> &'static str {
    let s = symbol.to_lowercase();
    if s.contains("abi_encode")
        || s.contains("abi_decode")
        || s.contains("write_word")
        || s.contains("read_word")
        || s.contains("payload_size")
        || s.contains("encode_field")
        || s.contains("decode_")
    {
        "ABI"
    } else if s.contains("contract_recv_abi")
        || s.contains("contract_init_abi")
        || s.contains("contract_runtime_root")
        || s.contains("main_root")
    {
        "Dispatch"
    } else if s.contains("set_base")
        || s.contains("set_pos")
        || s.contains("word_at")
        || (s.contains("base") && !s.contains("base_fee"))
        || s.contains("_pos")
        || s.contains("_len")
    {
        "Storage"
    } else {
        "Business"
    }
}

pub const FE_ELIMINATED_BY_DESIGN: &[&str] = &[
    "integer overflow (all arithmetic is checked by default)",
    "delegatecall (not supported in Fe)",
    "inheritance collision (Fe has no inheritance)",
    "tx.origin authentication (effect system prevents misuse)",
    "uninitialized storage (Fe initializes all storage)",
    "reentrancy via fallback (Fe contracts have explicit recv handlers)",
];

fn shortest_symbol(group: &[&FunctionRecord]) -> String {
    group
        .iter()
        .min_by_key(|f| f.symbol.len())
        .map(|f| f.symbol.clone())
        .unwrap_or_default()
}

#[cfg(feature = "datalog")]
impl crate::trace::CompilationTrace {
    pub fn from_source(source: &str) -> Result<(SourceAnalysis, Self), String> {
        let analysis = SourceAnalysis::from_source(source)?;
        let trace = Self::new();
        trace.ingest(&analysis.facts);
        Ok((analysis, trace))
    }
}
