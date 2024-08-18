fn assert_run(term: &str, input: &str, output: &str) {
    assert_eq!(bs::run(term, input.as_bytes()).unwrap(), output.as_bytes());
}

#[test]
fn end_to_end() {
    assert_run("id", "hello world", "hello world");
    assert_run("rev", "hello world", "dlrow olleh");
    assert_run("map inc", "hello world", "ifmmp!xpsme");
    assert_run(
        r"
          \input.
            let x = head input in
            let y = head (tail input) in
            let t = head (tail (tail input)) in
            let f = head (tail (tail (tail input))) in
            let res = (eq? x y) t f in
            cons res nil
        ",
        "00tf",
        "t",
    );
    assert_run(
        r"
          \input.
            let x = head input in
            let y = head (tail input) in
            let t = head (tail (tail input)) in
            let f = head (tail (tail (tail input))) in
            let res = (eq? x y) t f in
            cons res nil
        ",
        "01tf",
        "f",
    );
    // assert_run(r"\input.+ (head input) (head (tail input))", "01", "a");
}
