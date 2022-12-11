#[derive(Clone, Debug)]
enum Op {
    Add,
    Mul,
}

impl Op {
    fn apply(&self, modulo: i64, old: i64, val: Option<i64>) -> i64 {
        // (A + B) mod C = (A mod C + B mod C) mod C
        // (A * B) mod C = (A mod C * B mod C) mod C
        (match (self, val) {
            (Op::Add, Some(i)) => (old % modulo) + (i % modulo),
            (Op::Add, None) => (old % modulo) + (old % modulo),
            (Op::Mul, Some(i)) => (old % modulo) * (i % modulo),
            (Op::Mul, None) => (old % modulo) * (old % modulo),
        }) % modulo
    }
}

#[derive(Debug)]
struct Monkey {
    index: u64,
    items: Vec<i64>,
    operation: (Op, Option<i64>),
    test: i64,
    test_true: u64,
    test_false: u64,
}

impl std::str::FromStr for Monkey {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (s, index) = nom::sequence::delimited(
            nom::bytes::complete::tag("Monkey "),
            nom::character::complete::u64::<_, nom::error::VerboseError<_>>,
            nom::sequence::pair(
                nom::bytes::complete::tag(":"),
                nom::character::complete::line_ending,
            ),
        )(s)
        .unwrap();
        let (s, items) = nom::sequence::delimited(
            nom::bytes::complete::tag("  Starting items: "),
            nom::multi::separated_list1(
                nom::bytes::complete::tag(", "),
                nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
            ),
            nom::character::complete::line_ending,
        )(s)
        .unwrap();
        let (s, operation) = nom::sequence::delimited(
            nom::bytes::complete::tag("  Operation: new = old "),
            nom::sequence::separated_pair(
                nom::branch::alt((
                    nom::combinator::map(nom::bytes::complete::tag("+"), |_| Op::Add),
                    nom::combinator::map(nom::bytes::complete::tag("*"), |_| Op::Mul),
                )),
                nom::bytes::complete::tag(" "),
                nom::branch::alt((
                    nom::combinator::map(
                        nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
                        Some,
                    ),
                    nom::combinator::map(nom::bytes::complete::tag("old"), |_| None),
                )),
            ),
            nom::character::complete::line_ending,
        )(s)
        .unwrap();
        let (s, test) = nom::sequence::delimited(
            nom::bytes::complete::tag("  Test: divisible by "),
            nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
            nom::character::complete::line_ending,
        )(s)
        .unwrap();
        let (s, test_true) = nom::sequence::delimited(
            nom::bytes::complete::tag("    If true: throw to monkey "),
            nom::character::complete::u64::<_, nom::error::VerboseError<_>>,
            nom::character::complete::line_ending,
        )(s)
        .unwrap();
        let test_false = nom::sequence::delimited(
            nom::bytes::complete::tag("    If false: throw to monkey "),
            nom::character::complete::u64::<_, nom::error::VerboseError<_>>,
            nom::character::complete::line_ending,
        )(s)
        .unwrap()
        .1;
        Ok(Monkey {
            index,
            items,
            operation,
            test,
            test_true,
            test_false,
        })
    }
}

fn run_simulation(rounds: usize, worry: Option<i64>, monkeys: &[Monkey]) -> usize {
    let mut monkey_items: Vec<Vec<i64>> = monkeys.iter().map(|m| m.items.clone()).collect();
    let mut inspection_counts: Vec<usize> = monkeys.iter().map(|_| 0).collect();
    let fullmodulo = monkeys.iter().map(|m| m.test).product();
    for _ in 0..rounds {
        for (index, monkey) in monkeys.iter().enumerate() {
            assert!(monkey.index == index as u64); // just to double-check
            let items: Vec<i64> = monkey_items.get_mut(index).unwrap().drain(0..).collect();
            inspection_counts[index] += items.len();
            items.into_iter().for_each(|item: i64| {
                let item = monkey
                    .operation
                    .0
                    .apply(fullmodulo, item, monkey.operation.1);
                let item = match worry {
                    Some(w) => item / w,
                    None => item,
                };
                monkey_items[if (item % monkey.test) == 0 {
                    monkey.test_true
                } else {
                    monkey.test_false
                } as usize]
                    .push(item);
            });
        }
    }
    inspection_counts.sort();
    inspection_counts.iter().rev().take(2).product()
}

advent_2022::day_function!(11, input, {
    let groups: Vec<Vec<String>> = input.groups();
    let monkeys: Vec<Monkey> = groups
        .into_iter()
        .map(|s: Vec<String>| {
            s.into_iter()
                .map(|mut y: String| {
                    y.push('\n');
                    y
                })
                .collect::<Vec<String>>()
                .join("")
                .parse()
                .unwrap()
        })
        .collect();
    (
        run_simulation(20, Some(3), &monkeys).to_string(),
        run_simulation(10000, None, &monkeys).to_string(),
    )
});
