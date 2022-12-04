#[derive(Clone, Debug)]
struct Sack((String, String));

impl std::str::FromStr for Sack {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Sack((
            (s[0..s.len() / 2]).to_string(),
            (s[s.len() / 2..s.len()]).to_string(),
        )))
    }
}

fn group_to<T, I>(n: usize, mut i: I) -> Vec<Vec<T>>
where
    I: Iterator<Item = T>,
{
    if n < 1 {
        panic!();
    }
    let mut result = Vec::default();
    loop {
        match i.next() {
            None => return result,
            Some(x) => {
                let mut one = vec![x];
                for _ in 1..n {
                    one.push(i.next().unwrap());
                }
                result.push(one);
            }
        }
    }
}

fn value(c: char) -> u64 {
    if ('a'..='z').contains(&c) {
        return 1 + ((c as u64) - ('a' as u64));
    }
    if ('A'..='Z').contains(&c) {
        return 27 + ((c as u64) - ('A' as u64));
    }
    panic!()
}

advent_2022::day_function!(3, input, {
    let sacks: Vec<Sack> = input.lines();

    let result1: u64 = sacks
        .into_iter()
        .map(|Sack((l, r))| {
            let x: std::collections::HashSet<char> = l.chars().collect();
            let y: std::collections::HashSet<char> = r.chars().collect();
            value(*x.intersection(&y).next().unwrap())
        })
        .sum();

    let lines: Vec<String> = input.lines();
    let groups: Vec<Vec<String>> = group_to(3, lines.into_iter());
    let result2: u64 = groups
        .into_iter()
        .map(|xs| match xs.as_slice() {
            [] => 0,
            [s, rest @ ..] => {
                let mut x: std::collections::HashSet<char> = s.chars().collect();
                for y in rest.iter() {
                    let z: std::collections::HashSet<char> = y.chars().collect();
                    x.retain(|v| z.contains(v));
                }
                value(x.into_iter().next().unwrap())
            }
        })
        .sum();

    (result1.to_string(), result2.to_string())
});
