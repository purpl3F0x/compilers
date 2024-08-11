use std::{fs, process::Command};

/// Get the target directory of the current cargo build
fn get_cargo_target_dir() -> Result<std::path::PathBuf, Box<dyn std::error::Error>> {
    let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR")?);
    let profile = std::env::var("PROFILE")?;
    let mut target_dir = None;
    let mut sub_path = out_dir.as_path();
    while let Some(parent) = sub_path.parent() {
        if parent.ends_with(&profile) {
            target_dir = Some(parent);
            break;
        }
        sub_path = parent;
    }
    let target_dir = target_dir.ok_or("not found")?;
    Ok(target_dir.to_path_buf())
}

/// Build the standard library and rebuild libalan.a if one of it's source files have changed
fn main() -> shadow_rs::SdResult<()> {
    //* Build the standard library
    let dir: std::path::PathBuf = get_cargo_target_dir().unwrap();
    let _make_cmd = Command::new("make")
        .arg(format!("OUT_DIR={}", dir.display()))
        .current_dir("../stdlib")
        .spawn()
        .expect("Failed to build standard library");

    //* Rebuild libalan.a if one of it's source files have changed
    print!("cargo:rerun-if-changed=../stdlib/Makefile\n");
    let files = fs::read_dir("../stdlib")?;
    files
        .filter_map(|res| res.ok())
        .map(|dir_entry| dir_entry.path())
        .filter_map(|path| {
            if path
                .extension()
                .map_or(false, |ext| (ext == "c" || ext == "h"))
            {
                Some(path)
            } else {
                None
            }
        })
        .for_each(|f| print!("cargo:rerun-if-changed={}\n", f.display()));

    //* Run shadow
    shadow_rs::new()
}
