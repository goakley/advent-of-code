use clap::Parser;

mod day1;
mod day2;
mod day3;
mod day4;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    day: i64,
}

fn main() {
    let args = Args::parse();
    let input = advent_2022::Input::read(args.day);
    let (one, two) = match args.day {
        1 => day1::solve(input),
        2 => day2::solve(input),
        3 => day3::solve(input),
        4 => day4::solve(input),
        _ => todo!(),
    };
    println!("{}", one);
    println!("{}", two);
}
