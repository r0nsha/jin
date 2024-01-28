use clap::ValueEnum;

use crate::target::{TargetMetrics, TargetPlatform};

#[derive(Debug)]
pub struct BuildOptions {
    pub timings: bool,
    pub emit: Vec<EmitOption>,
    pub output_dir: String,
    pub target_platform: TargetPlatform,
    pub target_metrics: TargetMetrics,
}

impl BuildOptions {
    const DEFAULT_OUTPUT_DIR: &'static str = "build";

    pub fn new(
        timings: bool,
        emit: Vec<EmitOption>,
        output_dir: Option<String>,
        target_platform: TargetPlatform,
    ) -> Self {
        let target_metrics = target_platform.metrics();

        Self {
            timings,
            emit,
            output_dir: output_dir.unwrap_or(Self::DEFAULT_OUTPUT_DIR.to_string()),
            target_platform,
            target_metrics,
        }
    }

    pub fn should_emit(&self, opt: EmitOption) -> bool {
        self.emit.contains(&opt)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitOption {
    Ast,
    Hir,
    Mir,
    C,
}

impl EmitOption {
    pub fn as_extension(self) -> &'static str {
        match self {
            Self::Ast => "ast",
            Self::Hir => "hir",
            Self::Mir => "mir",
            Self::C => "c",
        }
    }
}
