#[derive(Debug, PartialEq, Eq)]
enum Instruction {
    Noop,
    Add(i64),
}

impl std::str::FromStr for Instruction {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(nom::branch::alt((
            nom::combinator::map(nom::bytes::complete::tag("noop"), |_: &str| {
                Instruction::Noop
            }),
            nom::combinator::map(
                nom::sequence::preceded(
                    nom::bytes::complete::tag("addx "),
                    nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
                ),
                Instruction::Add
            )
        ))(s).unwrap().1)
    }
}

advent_2022::day_function!(10, input, {
    let instructions: Vec<Instruction> = input.lines();
    let mut stages: Vec<i64> = Vec::with_capacity(instructions.len() * 2 + 1);
    let mut value = 1;
    stages.push(value);
    for i in instructions.iter() {
        match i {
            Instruction::Noop => { stages.push(value); },
            Instruction::Add(x) => { stages.push(value); stages.push(value); value += x; },
        }
    }
    let sum: i64 = stages.iter().enumerate().filter(|(i, _)| ((*i as i64) - 20) % 40 == 0).map(|(i, v)| (i as i64) * v).sum();
    let pixels: Vec<bool> = stages.iter().skip(1).enumerate().filter(|(i, _)| *i < 240).map(|(i, v)| (((i as i64) % 40) - v).abs() <= 1).collect();
    // ffs, the good iterator features are locked behind experimental/nightly
    let mut text: Vec<String> = Vec::new();
    for i in 0..6 {
        text.push(pixels.iter().skip(i * 40).take(40).map(|b| if *b { '#' } else { ' ' }).collect());
    }
    (sum.to_string(), text.join("\n"))
});
