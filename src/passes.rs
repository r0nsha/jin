mod analysis;
mod resolve;
pub mod subst;
mod typeck;

pub use analysis::analysis;
pub use resolve::resolve;
pub use typeck::typeck;
