mod analysis;
mod name_resolution;
pub mod subst;
mod typeck;

pub use analysis::analysis;
pub use name_resolution::name_resolution;
pub use typeck::typeck;
