fn main() {
    build_thrift("../SmallTest.thrift");
    build_thrift("../ThriftTest.thrift");
}

fn build_thrift(thriftfile: &'static str) {
    let thrift = std::env::var("THRIFT").unwrap_or("../../compiler/cpp/thrift".to_string());
    let target = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let mut cmd = std::process::Command::new(thrift);
    cmd.current_dir(target)
        .args(&["--gen", "rs"])
        .args(&["--out", "src"])
        .arg(thriftfile);
    let status = cmd.status().ok().expect("Could not execute thrift");
    if !status.success() {
        let code = match status.code() {
            Some(c) => format!("{}", c),
            None => "?".to_string(),
        };
        panic!("Thrift exited with code: {}", code);
    }
}
