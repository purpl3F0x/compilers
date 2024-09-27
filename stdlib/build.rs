use regex::Regex;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::Command as cmd;

fn find_llvm_as() -> Result<String, ()> {
    let candidates = ["llvm-as-18", "llvm-as"];

    for candidate in &candidates {
        if let Ok(output) = std::process::Command::new("which").arg(candidate).output() {
            if output.status.success() {
                let path = String::from_utf8_lossy(&output.stdout).trim().to_string();

                // Check the version of the found llvm-as
                if let Ok(version_output) = std::process::Command::new(&path).arg("--version").output() {
                    if version_output.status.success() {
                        let version_str = String::from_utf8_lossy(&version_output.stdout);

                        // Ensure it is version 18
                        if version_str.contains("version 18") {
                            return Ok(path);
                        }
                    }
                }
            }
        }
    }

    Err(())
}

fn process_file(file_path: &Path, regex: &Regex, substitution: &str) -> String {
    let contents = fs::read_to_string(file_path).expect("Failed to read the file");

    // Filter lines that match the regex pattern and apply the substitution
    let result = contents
        .lines()
        .filter_map(|line| if regex.is_match(line) { Some(regex.replace(line, substitution)) } else { None })
        .collect::<Vec<_>>()
        .join("\n");

    // Print the result
    print!("cargo:rerun-if-changed={}\n", file_path.display());

    return result;
}

fn main() {
    let out_path = std::env::var("OUT_DIR").expect("failed to get OUT_DIR");

    let libalan_ll_path = out_path.clone() + "/libalan.ll";
    let bit_file_path = out_path + "/libalan.bc";

    // Define the regex pattern and substitution string
    let regex = Regex::new(r"(?m)^define\s+dso_local\s+(?P<type>.+)\s+(?P<sig>@.+\)).+$").unwrap();
    let substitution = "declare $type $sig";

    let mut out_file = fs::File::create(&libalan_ll_path).expect("Failed to create the output file");

    // make .ll files

    cmd::new("make").arg("llvm-ir").output().expect("failed to execute process");

    // Iterate over all files in the executable's directory
    for entry in fs::read_dir(".").expect("Failed to read the directory") {
        let entry = entry.expect("Failed to get directory entry");
        let path = entry.path();

        // Check if the file has a `.bc` extension
        if path.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            let res = process_file(&path, &regex, substitution);

            // Write the result to the output file
            writeln!(&mut out_file, "\n; {}", path.file_name().unwrap().to_string_lossy()).unwrap();

            writeln!(&mut out_file, "{}", res).unwrap();
        }
    }

    //make bitcode file;
    let _cmd =
        cmd::new(find_llvm_as().unwrap()).arg("-o").arg(bit_file_path).arg(libalan_ll_path).output().expect("Couldn't generate libalan.bc");
}
