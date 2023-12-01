use std::env;
use std::fs;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/");
    let day_regex = regex::Regex::new(r"day([0-9]+)\.rs$").unwrap();
    let mut mods = "".to_string();
    let mut lines = "".to_string();
    for entity_r in std::fs::read_dir("src").unwrap() {
        let entity = entity_r.unwrap();
        if let Some(capture) = day_regex.captures(entity.path().to_str().unwrap()) {
            let num = capture.get(1).unwrap().as_str();
            mods = format!(
                "{}#[path=\"{}\"]\nmod day{};\n",
                mods,
                entity.path().canonicalize().unwrap().to_str().unwrap(),
                num
            );
            lines = format!("{}{} => day{}::solve(input),", lines, num, num);
        }
    }
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("main.rs");
    fs::write(
        &dest_path,
        format!(
            "{}
            advent::main!{{|_year, day, input| {{
                match day {{
                    {}
                    _ => todo!(),
                }}
            }}}}
            ",
            mods, lines,
        ),
    )
    .unwrap();
}
