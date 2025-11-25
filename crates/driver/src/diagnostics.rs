use std::ops::Range;

use camino::Utf8Path;
use codespan_reporting as cs;
use common::{
    InputDb,
    diagnostics::{LabelStyle, Severity},
    file::File,
};
use cs::{diagnostic as cs_diag, files as cs_files};
use hir::analysis::diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb};

pub trait ToCsDiag {
    fn to_cs(&self, db: &dyn SpannedInputDb) -> cs_diag::Diagnostic<File>;
}

pub trait SpannedInputDb: SpannedHirAnalysisDb + InputDb {}
impl<T> SpannedInputDb for T where T: SpannedHirAnalysisDb + InputDb {}

impl<T> ToCsDiag for T
where
    T: DiagnosticVoucher,
{
    fn to_cs(&self, db: &dyn SpannedInputDb) -> cs_diag::Diagnostic<File> {
        let complete = self.to_complete(db);

        let severity = convert_severity(complete.severity);
        let code = Some(complete.error_code.to_string());
        let message = complete.message;

        let labels = complete
            .sub_diagnostics
            .into_iter()
            .filter_map(|sub_diag| {
                let span = sub_diag.span?;
                match sub_diag.style {
                    LabelStyle::Primary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Primary, span.file, span.range)
                    }
                    LabelStyle::Secondary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Secondary, span.file, span.range)
                    }
                }
                .with_message(sub_diag.message)
                .into()
            })
            .collect();

        cs_diag::Diagnostic {
            severity,
            code,
            message,
            labels,
            notes: complete.notes,
        }
    }
}

fn convert_severity(severity: Severity) -> cs_diag::Severity {
    match severity {
        Severity::Error => cs_diag::Severity::Error,
        Severity::Warning => cs_diag::Severity::Warning,
        Severity::Note => cs_diag::Severity::Note,
    }
}

#[salsa::tracked(return_ref)]
pub fn file_line_starts(db: &dyn SpannedHirAnalysisDb, file: File) -> Vec<usize> {
    cs::files::line_starts(file.text(db)).collect()
}

pub struct CsDbWrapper<'a>(pub &'a dyn SpannedHirAnalysisDb);

impl<'db> cs_files::Files<'db> for CsDbWrapper<'db> {
    type FileId = File;
    type Name = &'db Utf8Path;
    type Source = &'db str;

    fn name(&'db self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        match file_id.path(self.0) {
            Some(path) => Ok(path.as_path()),
            None => Err(cs_files::Error::FileMissing),
        }
    }

    fn source(&'db self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self.0))
    }

    fn line_index(
        &'db self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = file_line_starts(self.0, file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'db self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, cs_files::Error> {
        let line_starts = file_line_starts(self.0, file_id);

        let start = *line_starts
            .get(line_index)
            .ok_or(cs_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len() - 1,
            })?;

        let end = if line_index == line_starts.len() - 1 {
            file_id.text(self.0).len()
        } else {
            *line_starts
                .get(line_index + 1)
                .ok_or(cs_files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len() - 1,
                })?
        };

        Ok(Range { start, end })
    }
}
