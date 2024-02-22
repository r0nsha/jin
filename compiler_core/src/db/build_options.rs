use crate::target::{TargetMetrics, TargetPlatform};

#[derive(Debug)]
pub struct BuildOptions {
    pub timings: bool,
    pub emit: Vec<EmitOption>,
    pub output_dir: Option<String>,
    pub target_platform: TargetPlatform,
    pub target_metrics: TargetMetrics,
}

impl BuildOptions {
    pub fn new(
        timings: bool,
        emit: Vec<EmitOption>,
        output_dir: Option<String>,
        target_platform: TargetPlatform,
    ) -> Self {
        Self {
            timings,
            emit,
            output_dir,
            target_metrics: target_platform.metrics(),
            target_platform,
        }
    }

    pub fn should_emit(&self, opt: EmitOption) -> bool {
        self.emit.contains(&opt)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
