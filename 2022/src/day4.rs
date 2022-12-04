// it LOOKS like Range<i64> would be good here, but it's not because the end is exclusive
#[derive(Clone, Debug)]
struct Pair(((i64, i64), (i64, i64)));

impl std::str::FromStr for Pair {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Pair(
            nom::sequence::separated_pair(
                nom::sequence::separated_pair(
                    nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
                    nom::bytes::complete::tag("-"),
                    nom::character::complete::i64,
                ),
                nom::bytes::complete::tag(","),
                nom::sequence::separated_pair(
                    nom::character::complete::i64,
                    nom::bytes::complete::tag("-"),
                    nom::character::complete::i64,
                ),
            )(s)
            .unwrap()
            .1,
        ))
    }
}

advent_2022::day_function!(2, input, {
    let pairs: Vec<Pair> = input.lines();

    let count: i64 = pairs
        .clone()
        .into_iter()
        .map(|Pair(((a, b), (x, y)))| {
            let ab = a..=b;
            let xy = x..=y;
            ((ab.contains(&x) && ab.contains(&y)) || (xy.contains(&a) && xy.contains(&b))) as i64
        })
        .sum();

    let overlaps: i64 = pairs
        .into_iter()
        .map(|Pair(((a, b), (x, y)))| {
            let ab = a..=b;
            let xy = x..=y;
            (ab.contains(&x) || ab.contains(&y) || xy.contains(&a) || xy.contains(&b)) as i64
        })
        .sum();

    (count.to_string(), overlaps.to_string())
});
