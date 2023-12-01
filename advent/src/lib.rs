mod input;

pub use crate::input::Input;

#[macro_export]
/// Run a solver on the CLI.
///
/// A solver is a function that accepts `(year: i64, day: i64, input: String)`
/// and returns a pair of `String`s (part 1 and part 2)
macro_rules! main(
    ($solver:expr) => (
        mod advent_of_code_main {
            use clap::Parser;
            #[derive(clap::Parser, Debug)]
            #[command(author, version, about, long_about = None)]
            /// The Advent of Code solver CLI.
            struct AdventOfCodeArgs {
                /// The AoC year
                year: i64,
                /// The AoC day
                day: i64,
            }
            pub fn main<F>(solver: F) where F: FnOnce(i64, i64, advent::Input) -> (String, String) {
                let args = AdventOfCodeArgs::parse();
                let input = advent::Input::read(args.year, args.day);
                let (one, two) = solver(args.year, args.day, input);
                println!("{}", one);
                println!("{}", two);
            }
        }
        fn main() {
            advent_of_code_main::main($solver)
        }
    )
);
