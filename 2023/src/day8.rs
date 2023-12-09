use std::collections::HashMap;

/// Greatest Common Divisor
fn gcd(first: usize, second: usize) -> usize {
    let mut max = first.max(second);
    let mut min = first.min(second);
    loop {
        let res = max % min;
        if res == 0 {
            return min;
        }
        max = min;
        min = res;
    }
}

/// Least Common Multiple
fn lcm(first: usize, second: usize) -> usize {
    first * second / gcd(first, second)
}

struct OverlapsIterator<T, U>(Vec<(U, T)>);

impl<A, U> FromIterator<A> for OverlapsIterator<A, U>
where
    A: Iterator<Item = U>,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = A>,
    {
        let v: Vec<_> = iter
            .into_iter()
            .filter_map(|mut t| t.next().map(|v| (v, t)))
            .collect();
        OverlapsIterator(v)
    }
}

impl<T, U> Iterator for OverlapsIterator<T, U>
where
    T: Iterator<Item = U>,
    U: Ord + Copy,
{
    type Item = T::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.iter().map(|(v, _)| v).max() {
            None => None,
            Some(found) => {
                let mut target: U = *found;
                loop {
                    let mut changed = false;
                    for (ref mut v, ref mut iter) in self.0.iter_mut() {
                        while v < &mut target {
                            match iter.next() {
                                None => return None,
                                Some(vv) => {
                                    *v = vv;
                                }
                            }
                        }
                        if v > &mut target {
                            target = *v;
                            changed = true;
                        }
                    }
                    if !changed {
                        for (v, t) in self.0.iter_mut() {
                            match t.next() {
                                None => {
                                    return None;
                                }
                                Some(vv) => {
                                    *v = vv;
                                }
                            }
                        }
                        return Some(target);
                    }
                }
            }
        }
    }
}

fn test_overlaps_iterator() {
    let one = vec![0, 5, 10, 15, 20, 25, 30].into_iter();
    let two = vec![1, 3, 5, 7, 9, 11, 13, 15, 17].into_iter();
    let three = vec![15, 30, 45].into_iter();
    let mut iter = OverlapsIterator::from_iter(vec![one, two, three]);
    assert_eq!(Some(15), iter.next());
    assert_eq!(None, iter.next());
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Direction {
    L,
    R,
}

impl Direction {
    fn select<A>(&self, (a, b): (A, A)) -> A {
        match self {
            Self::L => a,
            Self::R => b,
        }
    }
}

struct Visitation((Vec<usize>, Vec<usize>, usize));

impl Visitation {
    fn nth(&self, n: usize) -> usize {
        let (noncycled, cycled, cycle_length) = &self.0;
        match noncycled.get(n) {
            Some(n) => *n,
            None => {
                let i = (n - noncycled.len()) % cycled.len();
                let j = (n - noncycled.len()) / cycled.len();
                j * cycle_length + cycled[i]
            }
        }
    }

    fn iter(&self) -> VisitationIterator<'_> {
        VisitationIterator(self, 0)
    }
}

fn test_visitation() {
    let v = Visitation((vec![1], vec![3, 6], 6));
    assert_eq!(1, v.nth(0));
    assert_eq!(3, v.nth(1));
    assert_eq!(6, v.nth(2));
    assert_eq!(9, v.nth(3));
    assert_eq!(12, v.nth(4));
    assert_eq!(15, v.nth(5));
}

struct VisitationIterator<'a>(&'a Visitation, usize);

impl Iterator for VisitationIterator<'_> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.0.nth(self.1);
        self.1 += 1;
        Some(result)
    }
}

type Networks<'a> = HashMap<&'a str, (&'a str, &'a str)>;

/// Find all the nodes that the starting node can hit,
/// mapping from the node and its position in the direction array
/// to both the first index at which the node is encountered and its (possible) loop period
fn find_all_z_node_positions<'a>(
    directions: &[Direction],
    networks: &Networks<'a>,
    start: &'a str,
) -> Visitation {
    assert!(start.ends_with('A'));
    let mut current = start;
    let mut result: HashMap<(&'a str, usize), usize> = HashMap::default();
    let mut loop_index = 0;
    for (i, direction) in directions.iter().cycle().enumerate() {
        let key = (current, i % directions.len());
        if result.contains_key(&key) {
            loop_index = i;
            break;
        }
        result.insert(key, i);
        current = direction.select(networks[current]);
    }
    let loop_index_base = result[&(current, loop_index % directions.len())];
    let cycle_length = loop_index - loop_index_base;
    let (mut nonloops, mut loops): (Vec<usize>, Vec<usize>) = result
        .into_iter()
        .filter(|((k, _), _)| k.ends_with('Z'))
        .map(|(_, v)| v)
        .partition(|v| v < &loop_index_base);
    nonloops.sort();
    loops.sort();
    Visitation((nonloops, loops, cycle_length))
}

/// Solve part 2 lol
fn find_first_common_z_node_position(directions: &[Direction], networks: &Networks<'_>) -> usize {
    let visitations: Vec<Visitation> = networks
        .keys()
        .filter(|k| k.ends_with('A'))
        .map(|k| find_all_z_node_positions(directions, networks, k))
        .collect();
    let uniques: Vec<usize> = visitations
        .iter()
        .filter_map(
            |Visitation((noncycle, cycle, duty))| match (&noncycle[..], &cycle[..]) {
                ([], [x]) if x == duty => Some(*x),
                _ => None,
            },
        )
        .collect();
    // if there is a single element that cycles perfectly, use the lcm trick (super fast)
    if uniques.len() == visitations.len() {
        let mut iter = uniques.into_iter();
        match iter.next() {
            None => 0,
            Some(first) => iter.fold(first, lcm),
        }
    } else {
        let mut i = OverlapsIterator::from_iter(visitations.iter().map(|v| v.iter()));
        i.next().unwrap()
    }
}

fn parse_directions(input: &str) -> nom::IResult<&str, Vec<Direction>> {
    nom::sequence::terminated(
        nom::multi::many1(nom::branch::alt((
            nom::combinator::value(Direction::L, nom::character::complete::char('L')),
            nom::combinator::value(Direction::R, nom::character::complete::char('R')),
        ))),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_network(input: &str) -> nom::IResult<&str, (&str, (&str, &str))> {
    nom::sequence::terminated(
        nom::sequence::separated_pair(
            nom::character::complete::alphanumeric1,
            nom::sequence::delimited(
                nom::character::complete::space0,
                nom::character::complete::char('='),
                nom::character::complete::space0,
            ),
            nom::sequence::delimited(
                nom::character::complete::char('('),
                nom::sequence::separated_pair(
                    nom::character::complete::alphanumeric1,
                    nom::bytes::complete::tag(", "),
                    nom::character::complete::alphanumeric1,
                ),
                nom::character::complete::char(')'),
            ),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, (Vec<Direction>, Networks)> {
    nom::combinator::all_consuming(nom::sequence::separated_pair(
        parse_directions,
        nom::character::complete::line_ending,
        nom::combinator::map(nom::multi::many0(parse_network), |v| {
            v.into_iter().collect()
        }),
    ))(input)
}

advent_2023::day_function!(8, input, {
    test_overlaps_iterator();
    test_visitation();
    let (directions, networks) = parse_input(input.str()).unwrap().1;

    let mut current = "AAA";
    let mut result = 0;
    for (i, direction) in directions.iter().cycle().enumerate() {
        if current == "ZZZ" {
            result = i;
            break;
        }
        current = direction.select(networks[current]);
    }

    let result2 = find_first_common_z_node_position(&directions, &networks);

    (result.to_string(), result2.to_string())
});
