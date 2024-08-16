fn assert_run(term: &str, input: &str, output: &str) {
    assert_eq!(bs::run(term, input.as_bytes()).unwrap(), output.as_bytes());
}

#[test]
fn end_to_end() {
    assert_run("id", "hello world", "hello world");
    assert_run("rev", "hello world", "dlrow olleh");
}
