use ariadne::{Report, ReportBuilder, ReportKind};

use crate::span::Span;

pub type CompilerReport = Report<'static, Span>;
pub type CompilerReportBuilder = ReportBuilder<'static, Span>;

pub fn create_report(kind: ReportKind<'static>, span: &Span) -> CompilerReportBuilder {
    CompilerReport::build(kind, span.source_key(), span.start() as usize)
}
