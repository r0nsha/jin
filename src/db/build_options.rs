use clap::ValueEnum;

use crate::common::target::{TargetMetrics, TargetPlatform};

#[derive(Debug)]
pub struct BuildOptions {
    pub timings: bool,
    pub emit: Vec<EmitOption>,
    pub target_platform: TargetPlatform,
    pub target_metrics: TargetMetrics,
}

impl BuildOptions {
    pub fn new(timings: bool, emit: Vec<EmitOption>, target_platform: TargetPlatform) -> Self {
        let target_metrics = target_platform.metrics();
        Self { timings, emit, target_platform, target_metrics }
    }

    pub fn should_emit(&self, opt: EmitOption) -> bool {
        self.emit.contains(&opt)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitOption {
    Ast,
    TypedAst,
    Mir,
    LlvmIr,
}
