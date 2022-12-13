#[derive(Clone, Debug, PartialEq, Eq)]
enum Element {
    Value(i64),
    List(Vec<Element>),
}

fn nom_element(s: &str) -> nom::IResult<&str, Element> {
    nom::branch::alt((
        nom::combinator::map(nom::character::complete::i64, Element::Value),
        nom::combinator::map(
            nom::sequence::delimited(
                nom::character::complete::char('['),
                nom::multi::separated_list0(nom::character::complete::char(','), nom_element),
                nom::character::complete::char(']'),
            ),
            Element::List,
        ),
    ))(s)
}

impl std::str::FromStr for Element {
    type Err = nom::Err<nom::error::Error<String>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        nom_element(s).map(|(_, r)| r).map_err(|e| e.to_owned())
    }
}

impl Ord for Element {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Element::Value(a), Element::Value(b)) => a.cmp(b),
            (Element::Value(a), b) => Element::List(vec![Element::Value(*a)]).cmp(b),
            (a, Element::Value(b)) => a.cmp(&Element::List(vec![Element::Value(*b)])),
            (Element::List(a), Element::List(b)) => a
                .iter()
                .zip(b.iter())
                .find_map(|(a, b)| match a.cmp(b) {
                    std::cmp::Ordering::Equal => None,
                    o => Some(o),
                })
                .unwrap_or_else(|| a.len().cmp(&b.len())),
        }
    }
}

impl PartialOrd for Element {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

advent_2022::day_function!(13, input, {
    let divider_one: Element = Element::List(vec![Element::List(vec![Element::Value(2)])]);
    let divider_two: Element = Element::List(vec![Element::List(vec![Element::Value(6)])]);

    let lines: Vec<Vec<Element>> = input.groups();
    let pairs: Vec<(Element, Element)> = lines
        .into_iter()
        .map(|ls| {
            let mut iter = ls.into_iter();
            (iter.next().unwrap(), iter.next().unwrap())
        })
        .collect();
    let magic_sum: usize = pairs
        .iter()
        .enumerate()
        .filter_map(|(i, (a, b))| if a < b { Some(i + 1) } else { None })
        .sum();

    let mut list: Vec<Element> = pairs.into_iter().flat_map(|(a, b)| vec![a, b]).collect();
    list.push(divider_one.clone());
    list.push(divider_two.clone());
    list.sort();
    let one = list.binary_search(&divider_one).unwrap() + 1;
    let two = list.binary_search(&divider_two).unwrap() + 1;

    (magic_sum.to_string(), (one * two).to_string())
});
