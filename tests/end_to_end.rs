fn assert_run(term: &str, input: &str, output: &str) {
    assert_eq!(bs::run(term, input.as_bytes()).unwrap(), output.as_bytes());
}

#[test]
fn end_to_end() {
    assert_run(r"\x.x", "hello world", "hello world");
}
