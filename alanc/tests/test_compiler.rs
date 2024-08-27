#[cfg(test)]
mod tests {
    use std::{
        io::Write,
        process::{Command, Stdio},
    };
    use test_case::test_case;

    /// Macro to get the path of an example's source file
    macro_rules! examples_dir {
        ($filename:expr) => {
            format!("{}{}{}.alan", env!("CARGO_MANIFEST_DIR"), "/../examples/", $filename)
        };
    }

    /// Macro to get the path of an example's expected output
    macro_rules! result_dir {
        ($filename:expr) => {
            format!("{}{}{}.result", env!("CARGO_MANIFEST_DIR"), "/tests/outputs/", $filename)
        };
    }

    macro_rules! input_dir {
        ($filename:expr) => {
            format!("{}{}{}.input", env!("CARGO_MANIFEST_DIR"), "/tests/outputs/", $filename)
        };
    }

    #[test_case("hello", "hello"; "hello")]
    #[test_case("bsort", "bsort"; "bsort")]
    #[test_case("cancer", "cancer1"; "cancer-palindrome")]
    #[test_case("cancer", "cancer2"; "cancer-no-palindrome")]
    #[test_case("gcd", "gcd"; "gcd")]
    #[test_case("hanoi", "hanoi"; "hanoi")]

    fn compilation_test(filename: &str, output_filename: &str) {
        let src_file = examples_dir!(filename);
        let res_file = result_dir!(output_filename);
        let input_file = input_dir!(output_filename);

        let my_stdin = std::fs::read_to_string(input_file).unwrap_or("".to_string());

        let mut outfile_name = std::env::temp_dir();
        outfile_name.push(filename);
        outfile_name.set_extension("out");

        //* Make the compilation
        let cmd = Command::new(env!("CARGO_BIN_EXE_alanc"))
            .arg(src_file)
            .arg("-o")
            .arg(outfile_name.clone())
            .output()
            .expect("Failed to execute command");

        let _stdout = String::from_utf8_lossy(&cmd.stdout);
        let _stderr = String::from_utf8_lossy(&cmd.stderr);

        //*  Run the compiled file
        let mut run_cmd = Command::new(outfile_name)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to execute command");
        let stdin = run_cmd.stdin.as_mut().expect("Failed to open stdin");
        stdin.write_all(my_stdin.as_bytes()).expect("Failed to write to stdin");

        let run_cmd = run_cmd.wait_with_output().expect("Failed to execute command");

        let stdout = String::from_utf8_lossy(&run_cmd.stdout);
        let expected_output = std::fs::read_to_string(res_file).unwrap();

        assert_eq!(stdout, expected_output);
    }
}
