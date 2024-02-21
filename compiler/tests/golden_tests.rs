use goldentests::{TestConfig, TestResult};

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/jin", "goldentests", "# ")?;
    config.run_tests()
}
