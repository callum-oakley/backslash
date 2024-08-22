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
            let x = head input;
            let y = head (tail input);
            let t = head (tail (tail input));
            let f = head (tail (tail (tail input)));
            let res = (eq x y) t f;
            cons res nil
        ",
        "00tf",
        "t",
    );
    assert_run(
        r"
          \input.
            let x = head input;
            let y = head (tail input);
            let t = head (tail (tail input));
            let f = head (tail (tail (tail input)));
            let res = (lt x y) t f;
            cons res nil
        ",
        "01tf",
        "t",
    );
    assert_run(
        r"
          \input.
            let x = head input;
            let y = head (tail input);
            let res = add x y;
            cons res nil
        ",
        "01",
        "a",
    );
    assert_run(
        r"
          \input.
            let x = head input;
            let y = head (tail input);
            let res = sub x y;
            cons res nil
        ",
        "a0",
        "1",
    );
    assert_run(
        r"
          \input.
            let x = head input;
            let res = mul x x;
            cons res nil
        ",
        "\n",
        "d",
    );
    assert_run(r"map (add -32)", "backslash", "BACKSLASH");
    assert_run(
        r"
          \_.
            (and (eq 4 (div 42 10)) (eq 2 (rem 42 10)))
              (cons 84 nil)
              (cons 70 nil)
        ",
        "",
        "T",
    );
    assert_run(r"\_.intToString 42", "", "42");
    assert_run(r"\_.intToString 0", "", "0");
    assert_run(r"\_.intToString -42", "", "-42");
    assert_run(r"\input.intToString (inc (stringToInt input))", "42", "43");
    assert_run(r"\input.intToString (inc (stringToInt input))", "0", "1");
    assert_run(
        r"\input.intToString (inc (stringToInt input))",
        "-42",
        "-41",
    );
}
