use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

/// A single card, with joker wild rules optionally enabled
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
struct Card(bool, char);

impl Card {
    fn num(&self) -> u8 {
        match self.1 {
            '2' => 1,
            '3' => 2,
            '4' => 3,
            '5' => 4,
            '6' => 5,
            '7' => 6,
            '8' => 7,
            '9' => 8,
            'T' => 9,
            'J' => {
                if self.0 {
                    0
                } else {
                    10
                }
            }
            'Q' => 11,
            'K' => 12,
            'A' => 13,
            _ => panic!(""),
        }
    }
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.num().cmp(&other.num()))
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.num().cmp(&other.num())
    }
}

/// The classes of hand (what all the cards combined count as)
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Class {
    High,
    One,
    Two,
    Three,
    Full,
    Four,
    Five,
}

/// A hand of cards (5 cards with some associated score)
#[derive(Debug, PartialEq, Eq)]
struct Hand((Card, Card, Card, Card, Card));

impl Hand {
    /// How many of each card are in the hand
    fn count(&self) -> HashMap<char, u8> {
        let mut map = HashMap::default();
        map.entry(self.0 .0 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .1 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .2 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .3 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .4 .1).and_modify(|n| *n += 1).or_insert(1);
        map
    }

    /// The count of differerent unique cards in the hand, in count order
    ///
    /// (e.g. JAA9A -> [1,1,3])
    fn combo(&self) -> Vec<u8> {
        let mut values: Vec<u8> = self.count().into_values().collect();
        values.sort();
        values
    }

    /// The class of this hand
    fn class(&self) -> Class {
        match self.combo()[..] {
            [5] => Class::Five,
            [1, 4] => Class::Four,
            [2, 3] => Class::Full,
            [1, 1, 3] => Class::Three,
            [1, 2, 2] => Class::Two,
            [1, 1, 1, 2] => Class::One,
            [1, 1, 1, 1, 1] => Class::High,
            _ => panic!("impossible"),
        }
    }

    /// `cmp`, but pulled out to power both `PartialOrd` and `Ord`
    fn compare(&self, other: &Self) -> Ordering {
        let class_compare = self.class().cmp(&other.class());
        if class_compare != Ordering::Equal {
            class_compare
        } else {
            self.0.cmp(&other.0)
        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(other)
    }
}

/// A hand of cards (5 cards with some associated score) with jokers wild
#[derive(Debug, PartialEq, Eq)]
struct JokerHand((Card, Card, Card, Card, Card));

impl From<Hand> for JokerHand {
    fn from(hand: Hand) -> JokerHand {
        JokerHand((
            Card(true, hand.0 .0 .1),
            Card(true, hand.0 .1 .1),
            Card(true, hand.0 .2 .1),
            Card(true, hand.0 .3 .1),
            Card(true, hand.0 .4 .1),
        ))
    }
}

/// The count of differerent unique cards in all possible hands, in count order
///
/// Jokers can become any card, which means there are multiple possible hands,
/// and we generate all of them in this function.
///
/// (e.g. JAAAA -> [[1,4],[5]]
fn joker_combos(counts: &HashMap<char, u8>) -> Vec<Vec<u8>> {
    let mut values: Vec<u8> = counts
        .values()
        .filter_map(|n| if *n > 0 { Some(*n) } else { None })
        .collect();
    values.sort();
    let mut result: Vec<Vec<u8>> = vec![values];
    let jokers = *counts.get(&'J').unwrap_or(&0);
    if jokers > 0 {
        let eligibles: HashSet<char> = counts
            .keys()
            .filter_map(|k| if k == &'J' { None } else { Some(*k) })
            .collect();
        for c in eligibles.into_iter() {
            let cs: HashMap<char, u8> = counts
                .clone()
                .into_iter()
                .map(|(k, n)| {
                    (
                        k,
                        match k {
                            'J' => n - 1,
                            x if x == c => n + 1,
                            _ => n,
                        },
                    )
                })
                .collect();
            let mut recursion = joker_combos(&cs);
            result.append(&mut recursion);
        }
    }
    result
}

impl JokerHand {
    /// How many of each card are in the hand
    fn count(&self) -> HashMap<char, u8> {
        let mut map = HashMap::default();
        map.entry(self.0 .0 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .1 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .2 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .3 .1).and_modify(|n| *n += 1).or_insert(1);
        map.entry(self.0 .4 .1).and_modify(|n| *n += 1).or_insert(1);
        map
    }

    /// The class of this hand
    fn class(&self) -> Class {
        let all_combos = joker_combos(&self.count());
        all_combos
            .into_iter()
            .map(|combos| match combos[..] {
                [5] => Class::Five,
                [1, 4] => Class::Four,
                [2, 3] => Class::Full,
                [1, 1, 3] => Class::Three,
                [1, 2, 2] => Class::Two,
                [1, 1, 1, 2] => Class::One,
                [1, 1, 1, 1, 1] => Class::High,
                _ => panic!("impossible"),
            })
            .max()
            .expect("impossible")
    }

    /// `cmp`, but pulled out to power both `PartialOrd` and `Ord`
    fn compare(&self, other: &Self) -> Ordering {
        let class_compare = self.class().cmp(&other.class());
        if class_compare != Ordering::Equal {
            class_compare
        } else {
            self.0.cmp(&other.0)
        }
    }
}

impl PartialOrd for JokerHand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl Ord for JokerHand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(other)
    }
}

fn parse_cards(input: &str) -> nom::IResult<&str, Hand> {
    nom::combinator::map(
        nom::sequence::tuple((
            nom::combinator::map(nom::character::complete::anychar, |c| Card(false, c)),
            nom::combinator::map(nom::character::complete::anychar, |c| Card(false, c)),
            nom::combinator::map(nom::character::complete::anychar, |c| Card(false, c)),
            nom::combinator::map(nom::character::complete::anychar, |c| Card(false, c)),
            nom::combinator::map(nom::character::complete::anychar, |c| Card(false, c)),
        )),
        Hand,
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, Vec<(Hand, u64)>> {
    nom::combinator::all_consuming(nom::multi::many0(nom::sequence::terminated(
        nom::sequence::separated_pair(
            parse_cards,
            nom::character::complete::space1,
            nom::character::complete::u64,
        ),
        nom::character::complete::line_ending,
    )))(input)
}

advent_2023::day_function!(7, input, {
    let mut hands = parse_input(input.str()).unwrap().1;
    hands.sort();
    let total_winnings: u64 = hands
        .iter()
        .enumerate()
        .map(|(i, (_, bet))| (1 + i as u64) * bet)
        .sum();

    let mut joker_hands: Vec<(JokerHand, u64)> = hands
        .into_iter()
        .map(|(hand, bid)| (JokerHand::from(hand), bid))
        .collect();
    joker_hands.sort();
    let total_joker_winnings: u64 = joker_hands
        .iter()
        .enumerate()
        .map(|(i, (_, bet))| (1 + i as u64) * bet)
        .sum();

    (total_winnings.to_string(), total_joker_winnings.to_string())
});
