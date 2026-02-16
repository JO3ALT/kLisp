use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

#[derive(Clone, Debug)]
struct Case {
    name: String,
    program: String,
    query: String,
    vars: Vec<String>,
}

fn run_with_input(cmd: &str, args: &[&str], input: &str) -> Result<String, String> {
    let mut child = Command::new(cmd)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to spawn {cmd}: {e}"))?;

    if let Some(stdin) = child.stdin.as_mut() {
        use std::io::Write;
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| format!("failed to write stdin for {cmd}: {e}"))?;
    }

    let out = child
        .wait_with_output()
        .map_err(|e| format!("failed waiting {cmd}: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "{cmd} failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).to_string())
}

fn normalize_solution_line(line: &str) -> String {
    let line = line.trim();
    if line == "true." || line == "false." {
        return line.to_string();
    }
    let body = line.strip_suffix('.').unwrap_or(line);
    let mut parts = body
        .split(',')
        .map(str::trim)
        .filter(|p| !p.is_empty())
        .filter_map(|p| {
            if let Some((lhs, rhs)) = p.split_once(" = ") {
                if lhs.trim() == rhs.trim() {
                    return None;
                }
            }
            Some(p.to_string())
        })
        .collect::<Vec<_>>();
    if parts.is_empty() {
        return "true.".to_string();
    }
    parts.sort();
    format!("{}.", parts.join(", "))
}

fn normalize_lines(s: &str) -> Vec<String> {
    let mut v = s
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .map(|l| l.to_ascii_lowercase())
        .map(|l| normalize_solution_line(&l))
        .collect::<Vec<_>>();
    v.sort();
    v
}

fn run_lisp_engine(engine: &str, script_path: &str) -> Result<String, String> {
    let mut cmd = Command::new(engine);
    match engine {
        "sbcl" => {
            cmd.arg("--script").arg(script_path);
        }
        "ecl" => {
            cmd.arg("-shell").arg(script_path);
        }
        other => return Err(format!("unsupported lisp engine: {other}")),
    }
    let out = cmd
        .output()
        .map_err(|e| format!("failed to run {engine}: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "{engine} failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).to_string())
}

fn extract_solver_lines(stdout: &str) -> String {
    let mut out = String::new();
    for line in stdout.lines() {
        let t = line.trim();
        if t.is_empty() {
            continue;
        }
        if t.starts_with('(') || matches!(t, "t" | "T" | "nil" | "NIL") {
            out.push_str(t);
            out.push('\n');
        }
    }
    out
}

fn run_klisp_pipeline(program: &str) -> Result<String, String> {
    let klisp_bin = env!("CARGO_BIN_EXE_klisp");
    let combined = format!("{}\n{}", fs::read_to_string("prolog-cl.lisp").map_err(|e| e.to_string())?, program);
    run_with_input(klisp_bin, &[], &combined)
}

fn run_fprolog_pipeline(case: &Case, lisp_engine: &str) -> Result<Vec<String>, String> {
    let fprolog_bin = "fprolog";
    let frontend_out = run_with_input(fprolog_bin, &[], &case.program)?;

    if lisp_engine == "klisp" {
        let klisp_out = run_klisp_pipeline(&frontend_out)?;
        let awk_out = run_with_input("awk", &["-f", "sexp2prolog_stream.awk"], &klisp_out)?;
        return Ok(normalize_lines(&awk_out));
    }

    let lisp_driver = format!("(load \"prolog-cl.lisp\")\n{}\n", frontend_out);
    let script_path = format!(
        "/tmp/fprolog_case_{}_{}_{}.lisp",
        std::process::id(),
        lisp_engine,
        case.name
    );
    fs::write(&script_path, lisp_driver)
        .map_err(|e| format!("failed to write lisp driver: {e}"))?;

    let lisp_out = run_lisp_engine(lisp_engine, &script_path)?;
    let _ = fs::remove_file(&script_path);
    let solver_lines = extract_solver_lines(&lisp_out);

    let awk_out = run_with_input("awk", &["-f", "sexp2prolog_stream.awk"], &solver_lines)?;

    Ok(normalize_lines(&awk_out))
}

fn run_swipl(case: &Case) -> Result<Vec<String>, String> {
    let bindings_expr = if case.vars.is_empty() {
        "[]".to_string()
    } else {
        format!(
            "[{}]",
            case.vars
                .iter()
                .map(|v| format!("'{}'={}", v, v))
                .collect::<Vec<_>>()
                .join(",")
        )
    };

    let kb = case
        .program
        .lines()
        .filter(|l| !l.trim_start().starts_with("?-"))
        .collect::<Vec<_>>()
        .join("\n");

    let program = format!(
        r#"
{kb}

:- dynamic found/1.

print_bindings([]) :-
    write('true.'), nl, !.
print_bindings(Bs) :-
    print_pairs(Bs).

print_pairs([N=V]) :-
    write(N), write(' = '), write_term(V, [quoted(false)]), write('.'), nl, !.
print_pairs([N=V|R]) :-
    write(N), write(' = '), write_term(V, [quoted(false)]), write(', '),
    print_pairs(R).

run_case :-
    retractall(found(_)),
    asserta(found(0)),
    (   ({query}),
        retract(found(C0)), C1 is C0 + 1, asserta(found(C1)),
        print_bindings({bindings}),
        fail
    ;   true
    ),
    found(C),
    (C =:= 0 -> write('false.'), nl ; true),
    halt.

:- initialization(run_case).
"#,
        kb = kb,
        query = case.query,
        bindings = bindings_expr
    );

    let path = format!("/tmp/swipl_case_{}_{}.pl", std::process::id(), case.name);
    fs::write(&path, program).map_err(|e| format!("failed to write swipl file: {e}"))?;
    let out = Command::new("swipl")
        .arg("-q")
        .arg("-f")
        .arg(&path)
        .output()
        .map_err(|e| format!("failed to run swipl: {e}"))?;
    let _ = fs::remove_file(&path);

    if !out.status.success() {
        return Err(format!(
            "swipl failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ));
    }

    Ok(normalize_lines(&String::from_utf8_lossy(&out.stdout)))
}

fn extract_query(program: &str) -> Result<String, String> {
    for line in program.lines() {
        let t = line.trim();
        if let Some(rest) = t.strip_prefix("?-") {
            let q = rest.trim().trim_end_matches('.').trim();
            if q.is_empty() {
                return Err("empty query after '?-'".to_string());
            }
            return Ok(q.to_string());
        }
    }
    Err("no query line ('?- ... .') found".to_string())
}

fn extract_vars(query: &str) -> Vec<String> {
    let mut vars = Vec::new();
    let mut i = 0;
    let bs = query.as_bytes();
    while i < bs.len() {
        let c = bs[i] as char;
        if c == '\'' {
            i += 1;
            while i < bs.len() {
                let ch = bs[i] as char;
                if ch == '\'' {
                    if i + 1 < bs.len() && bs[i + 1] as char == '\'' {
                        i += 2;
                        continue;
                    }
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if c.is_ascii_alphabetic() || c == '_' {
            let start = i;
            i += 1;
            while i < bs.len() {
                let ch = bs[i] as char;
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    i += 1;
                } else {
                    break;
                }
            }
            let tok = &query[start..i];
            let first = tok.chars().next().unwrap_or('a');
            if (first.is_ascii_uppercase() || first == '_') && tok != "_" {
                if !vars.iter().any(|v| v == tok) {
                    vars.push(tok.to_string());
                }
            }
            continue;
        }

        i += 1;
    }
    vars
}

fn load_cases_from_dir(dir: &str) -> Result<Vec<Case>, String> {
    let mut entries = fs::read_dir(dir)
        .map_err(|e| format!("failed to read {dir}: {e}"))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| format!("failed to list {dir}: {e}"))?;
    entries.sort_by_key(|e| e.file_name());

    let mut cases = Vec::new();
    for e in entries {
        let path = e.path();
        if path.extension().and_then(|s| s.to_str()) != Some("pl") {
            continue;
        }
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| format!("bad filename: {}", path.display()))?
            .to_string();
        let program = fs::read_to_string(&path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        let query = extract_query(&program)
            .map_err(|e| format!("{}: {e}", path.display()))?;
        let vars = extract_vars(&query);
        cases.push(Case {
            name,
            program,
            query,
            vars,
        });
    }

    if cases.is_empty() {
        return Err(format!("no .pl cases found in {dir}"));
    }

    Ok(cases)
}

#[test]
fn compare_fprolog_awk_with_swipl() {
    if Command::new("fprolog").arg("--help").output().is_err() {
        eprintln!("skip: fprolog not found in PATH");
        return;
    }
    if Command::new("sbcl").arg("--version").output().is_err() {
        eprintln!("skip: sbcl not found");
        return;
    }
    if Command::new("ecl").arg("--version").output().is_err() {
        eprintln!("skip: ecl not found");
        return;
    }
    if Command::new("swipl").arg("--version").output().is_err() {
        eprintln!("skip: swipl not found");
        return;
    }
    if !Path::new(env!("CARGO_BIN_EXE_klisp")).exists() {
        eprintln!("skip: klisp binary not found");
        return;
    }

    let cases = load_cases_from_dir("tests/cases").expect("failed to load test cases");
    for case in cases {
        let sbcl = run_fprolog_pipeline(&case, "sbcl").unwrap_or_else(|e| {
            panic!("SBCL pipeline failed for {}: {e}", case.name);
        });
        let ecl = run_fprolog_pipeline(&case, "ecl").unwrap_or_else(|e| {
            panic!("ECL pipeline failed for {}: {e}", case.name);
        });
        let klisp = run_fprolog_pipeline(&case, "klisp").unwrap_or_else(|e| {
            panic!("klisp pipeline failed for {}: {e}", case.name);
        });
        let rhs = run_swipl(&case).unwrap_or_else(|e| {
            panic!("swipl failed for {}: {e}", case.name);
        });

        assert_eq!(sbcl, rhs, "SBCL mismatch for case {}", case.name);
        assert_eq!(ecl, rhs, "ECL mismatch for case {}", case.name);
        assert_eq!(klisp, rhs, "klisp mismatch for case {}", case.name);
    }
}
