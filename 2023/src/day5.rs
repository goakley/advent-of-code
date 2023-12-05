/// The result of detecting overlap between two ranges;
/// the ranges (before, during, after) the target range
type OverlapResult = (Option<(i64, i64)>, Option<(i64, i64)>, Option<(i64, i64)>);

/// Using fixed window `(x,y)`, get the before, overlap, and after parts for `(a,b)`
fn overlap((a, b): (i64, i64), (x, y): (i64, i64)) -> OverlapResult {
    if b < a || y < x {
        panic!("invalid range");
    }
    (
        (if a < x { Some((a, b.min(x - 1))) } else { None }),
        (if b > x && a < y {
            Some((a.max(x), b.min(y)))
        } else {
            None
        }),
        (if b > y { Some((a.max(y + 1), b)) } else { None }),
    )
}

fn test_overlap() {
    assert_eq!((Some((1, 14)), None, None), overlap((1, 14), (20, 30)));
    assert_eq!((None, None, Some((40, 60))), overlap((40, 60), (20, 30)));
    assert_eq!(
        (Some((15, 19)), Some((20, 25)), None),
        overlap((15, 25), (20, 30))
    );
    assert_eq!(
        (None, Some((25, 30)), Some((31, 35))),
        overlap((25, 35), (20, 30))
    );
    assert_eq!((None, Some((40, 60)), None), overlap((40, 60), (10, 90)));
    assert_eq!(
        (Some((20, 44)), Some((45, 52)), Some((53, 80))),
        overlap((20, 80), (45, 52))
    );
    assert_eq!((None, Some((30, 30)), None), overlap((30, 30), (20, 40)));
}

type Entry = (i64, i64, i64);
#[derive(Debug)]
struct MapEntry<'a>(((&'a str, &'a str), Vec<Entry>));

impl MapEntry<'_> {
    fn source(&self) -> &str {
        self.0 .0 .0
    }

    fn destination(&self) -> &str {
        self.0 .0 .1
    }

    /// Map a source value to its destination value
    fn map(&self, value: i64) -> i64 {
        for (destination, source, length) in self.0 .1.iter() {
            let offset = value - source;
            if offset >= 0 && offset < *length {
                return destination + offset;
            }
        }
        value
    }

    /// Map a source range to its destination range(s)
    fn map_range(&self, mut min: i64, max: i64) -> Vec<(i64, i64)> {
        let mut sorted_ranges: Vec<Entry> = self.0 .1.clone();
        sorted_ranges.sort_by_key(|(_, d, _)| *d);
        let mut ranges = vec![];
        for (destination, source, length) in sorted_ranges.iter() {
            let (before, during, after) = overlap((min, max), (*source, source + length - 1));
            if let Some(range) = before {
                // outside of the range means no offset
                ranges.push(range);
            }
            if let Some((l, r)) = during {
                // inside of the range means offset the result to the destination
                let offset = destination - source;
                ranges.push((l + offset, r + offset));
            }
            if let Some((l, r)) = after {
                // cut the old beginning to prepare to try future ranges
                min = l;
                assert!(max == r);
                if min > max {
                    return ranges;
                }
            } else {
                return ranges;
            }
        }
        if min <= max {
            // make sure any leftover range is included
            ranges.push((min, max));
        }
        ranges
    }
}

fn parse_seeds(input: &str) -> nom::IResult<&str, Vec<i64>> {
    nom::sequence::terminated(
        nom::sequence::preceded(
            nom::bytes::complete::tag("seeds: "),
            nom::multi::separated_list0(
                nom::character::complete::space1,
                nom::character::complete::i64,
            ),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_entry(input: &str) -> nom::IResult<&str, Entry> {
    nom::sequence::tuple((
        nom::character::complete::i64,
        nom::sequence::delimited(
            nom::character::complete::space1,
            nom::character::complete::i64,
            nom::character::complete::space1,
        ),
        nom::character::complete::i64,
    ))(input)
}

fn parse_map_header(input: &str) -> nom::IResult<&str, (&str, &str)> {
    nom::sequence::terminated(
        nom::sequence::terminated(
            nom::sequence::separated_pair(
                nom::character::complete::alphanumeric1,
                nom::bytes::complete::tag("-to-"),
                nom::character::complete::alphanumeric1,
            ),
            nom::bytes::complete::tag(" map:"),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_map_entry(input: &str) -> nom::IResult<&str, MapEntry> {
    nom::combinator::map(
        nom::sequence::pair(
            parse_map_header,
            nom::multi::many0(nom::sequence::terminated(
                parse_entry,
                nom::character::complete::line_ending,
            )),
        ),
        MapEntry,
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, (Vec<i64>, Vec<MapEntry>)> {
    nom::combinator::all_consuming(nom::sequence::separated_pair(
        parse_seeds,
        nom::character::complete::line_ending,
        nom::multi::separated_list0(nom::character::complete::line_ending, parse_map_entry),
    ))(input)
}

advent_2023::day_function!(5, input, {
    test_overlap();
    let (seeds, map_entries) = parse_input(input.str()).unwrap().1;

    let mut values = seeds.clone();
    let mut source = "seed";
    while source != "location" {
        let map_entry = map_entries
            .iter()
            .find(|map_entry| map_entry.source() == source)
            .unwrap();
        values = values.into_iter().map(|i| map_entry.map(i)).collect();
        source = map_entry.destination();
    }
    let lowest_location = values.iter().min().unwrap();

    let mut ranges: Vec<(i64, i64)> = (0..seeds.len())
        .step_by(2)
        .map(|i| (seeds[i], seeds[i] + seeds[i + 1] - 1))
        .collect();
    let mut source = "seed";
    while source != "location" {
        let map_entry = map_entries
            .iter()
            .find(|map_entry| map_entry.source() == source)
            .unwrap();
        ranges = ranges
            .iter()
            .flat_map(|(min, max)| map_entry.map_range(*min, *max))
            .collect();
        source = map_entry.destination();
    }
    let range_lowest_location = ranges.iter().map(|(min, _)| min).min().unwrap();

    (
        lowest_location.to_string(),
        range_lowest_location.to_string(),
    )
});
