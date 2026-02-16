use std::process::Command;

#[test]
fn prolog_cl_smoke() {
    let has_sbcl = Command::new("sbcl").arg("--version").output();
    if has_sbcl.is_err() {
        eprintln!("skip: sbcl not found in PATH");
        return;
    }

    let out = Command::new("sbcl")
        .arg("--script")
        .arg("tests/prolog_cl_smoke.lisp")
        .output()
        .expect("failed to run sbcl script");

    if !out.status.success() {
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        panic!(
            "sbcl smoke failed\nstdout:\n{}\nstderr:\n{}",
            stdout, stderr
        );
    }

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("PROLOG-CL-SMOKE: OK"),
        "unexpected sbcl output: {stdout}"
    );
}
