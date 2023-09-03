pub type Adjustments = Vec<Adjustment>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Adjustment {
    NeverCoercion,
}
