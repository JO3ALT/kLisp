use std::process::Command;

fn run_lisp_impl(cmd: &str, args: &[&str]) -> Result<String, String> {
    let out = Command::new(cmd)
        .args(args)
        .output()
        .map_err(|e| format!("failed to run {cmd}: {e}"))?;

    if !out.status.success() {
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        return Err(format!(
            "{cmd} failed\nstdout:\n{}\nstderr:\n{}",
            stdout, stderr
        ));
    }

    let text = String::from_utf8_lossy(&out.stdout);
    let filtered = text
        .lines()
        .filter(|line| line.starts_with('s'))
        .collect::<Vec<_>>()
        .join("\n");
    Ok(filtered)
}

#[test]
fn prolog_samples_match_sbcl_and_ecl() {
    if Command::new("sbcl").arg("--version").output().is_err() {
        eprintln!("skip: sbcl not found in PATH");
        return;
    }
    if Command::new("ecl").arg("--version").output().is_err() {
        eprintln!("skip: ecl not found in PATH");
        return;
    }

    let sbcl = run_lisp_impl("sbcl", &["--script", "tests/prolog_samples_compare.lisp"])
        .expect("sbcl run should succeed");
    let ecl = run_lisp_impl("ecl", &["-shell", "tests/prolog_samples_compare.lisp"])
        .expect("ecl run should succeed");

    assert_eq!(
        sbcl, ecl,
        "SBCL and ECL outputs differ on sample program set"
    );
}
