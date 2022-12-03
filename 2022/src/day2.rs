#[derive(PartialEq, Eq)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

impl std::str::FromStr for Hand {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "A" | "X" => Hand::Rock,
            "B" | "Y" => Hand::Paper,
            "C" | "Z" => Hand::Scissors,
            _ => panic!(),
        })
    }
}

enum Victor {
    Lose,
    Draw,
    Win,
}

impl std::str::FromStr for Victor {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "X" => Victor::Lose,
            "Y" => Victor::Draw,
            "Z" => Victor::Win,
            _ => panic!(),
        })
    }
}

fn score((a, b): (&Hand, &Hand)) -> i64 {
    let base = match b {
        Hand::Rock => 1,
        Hand::Paper => 2,
        Hand::Scissors => 3,
    };
    if a == b {
        return base + 3;
    }
    base + match (a, b) {
        (Hand::Rock, Hand::Paper) => 6,
        (Hand::Paper, Hand::Scissors) => 6,
        (Hand::Scissors, Hand::Rock) => 6,
        _ => 0,
    }
}

fn play((a, b): (&Hand, &Victor)) -> i64 {
    let myhand = match (a, b) {
        (Hand::Rock, Victor::Lose) => Hand::Scissors,
        (Hand::Paper, Victor::Lose) => Hand::Rock,
        (Hand::Scissors, Victor::Lose) => Hand::Paper,
        (Hand::Rock, Victor::Draw) => Hand::Rock,
        (Hand::Paper, Victor::Draw) => Hand::Paper,
        (Hand::Scissors, Victor::Draw) => Hand::Scissors,
        (Hand::Rock, Victor::Win) => Hand::Paper,
        (Hand::Paper, Victor::Win) => Hand::Scissors,
        (Hand::Scissors, Victor::Win) => Hand::Rock,
    };
    score((a, &myhand))
}

advent_2022::day_function!(2, input, {
    let hands1: Vec<(Hand, Hand)> = input.pairs();
    let sum: i64 = hands1.iter().map(|(a, b)| score((a, b))).sum();

    let hands2: Vec<(Hand, Victor)> = input.pairs();
    let sum2: i64 = hands2.iter().map(|(a, b)| play((a, b))).sum();

    (sum.to_string(), sum2.to_string())
});
