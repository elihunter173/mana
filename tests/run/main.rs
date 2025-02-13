fn run(code: &str, expected: &str) {
    let result = mana::run_code(code);
    assert_eq!(result, expected)
}

#[test]
fn literal() {
    run(
        r#"
def main = fn(): Int {
    1
}
"#,
        "1",
    );
}

#[test]
fn add() {
    run(
        r#"
def main = fn(): Int {
    1 + 2
}
"#,
        "3",
    );
}

#[test]
fn assign() {
    run(
        r#"
def main = fn(): Int {
    let a = {
        let b = 2
        b + 2
    }
    a + 1
}
"#,
        "5",
    );
}

#[test]
#[ignore = "TODO: fix variable assignment"]
fn simple_loop() {
    run(
        r#"
def main = fn(): Int {
    let n = 0
    while n < 5 {
        n += 1
    }
    n
}
"#,
        "5",
    );
}
