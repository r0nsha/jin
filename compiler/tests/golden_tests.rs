use goldentests::{TestConfig, TestResult};

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/jin", "tests", "# ")?;
    config.run_tests()
}
