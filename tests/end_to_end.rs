static STD: &str = include_str!("../std.bs");

fn assert_run(term: &str, input: &str, output: &str) {
    let mut full_term = STD.to_owned();
    full_term.push('\n');
    full_term.push_str(term);
    assert_eq!(
        bs::run(&full_term, input.as_bytes()).unwrap(),
        output.as_bytes()
    );
}

#[test]
fn end_to_end() {
    assert_run("id", "hello world", "hello world");
    assert_run("rev", "hello world", "dlrow olleh");
}
