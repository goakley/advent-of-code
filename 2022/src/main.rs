use clap::Parser;

include!(concat!(env!("OUT_DIR"), "/gen.rs"));

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    day: i64,
}

fn main() {
    let args = Args::parse();
    let input = advent_2022::Input::read(args.day);
    let (one, two) = solve(input, args.day);
    println!("{}", one);
    println!("{}", two);
}
