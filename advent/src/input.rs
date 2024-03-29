use nom::Finish;
use std::collections::HashMap;
use std::io::Read;
use std::path::PathBuf;

/// An input (example or real) for one day of advent.
pub struct Input {
    data: Vec<u8>,
}

impl std::fmt::Debug for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.data.fmt(f)
    }
}

impl Input {
    /// Reads the input for a given day.
    ///
    /// This method will attempt to read from stdin.
    /// If there is no data on stdin, it will instead read the real input from the Advent of Code website (assuming you're logged in).
    pub fn read(year: i64, day: i64) -> Self {
        let shelf = Self::read_stdin();
        if shelf.data.is_empty() {
            Self::read_day(year, day)
        } else {
            shelf
        }
    }

    /// Reads the input from stdin.
    pub fn read_stdin() -> Self {
        let mut data = Vec::new();
        std::io::stdin().read_to_end(&mut data).unwrap();
        Input { data }
    }

    /// Reads the real input from the Advent of Code website.
    pub fn read_day(year: i64, day: i64) -> Self {
        let path = PathBuf::from(format!("input/{:?}", year));
        std::fs::create_dir_all(&path).unwrap();
        let path = path.join(format!("{:?}", day));
        match std::fs::read(&path) {
            Ok(data) => Input { data },
            Err(e) => match e.kind() {
                std::io::ErrorKind::NotFound => {
                    let cookies_results: Vec<bench_scraper::KnownBrowserCookies> =
                        bench_scraper::find_cookies().unwrap();
                    let jar: reqwest::cookie::Jar = cookies_results
                        .into_iter()
                        .flat_map(|kbcookies| kbcookies.cookies)
                        .collect();
                    let builder = reqwest::blocking::Client::builder()
                        .user_agent("bandcamper")
                        //.gzip(true)
                        .cookie_store(true)
                        .cookie_provider(std::sync::Arc::new(jar));
                    let client = builder.build().unwrap();
                    let data = client
                        .get(format!(
                            "https://adventofcode.com/{:?}/day/{:?}/input",
                            year, day
                        ))
                        .send()
                        .and_then(|r| r.error_for_status())
                        .unwrap()
                        .bytes()
                        .unwrap()
                        .into();
                    std::fs::write(path, &data).unwrap();
                    Input { data }
                }
                _ => panic!("{:?}", e),
            },
        }
    }

    /// Gets a reference to the input as a string.
    pub fn str(&self) -> &str {
        std::str::from_utf8(&self.data).unwrap()
    }

    /// Gets a reference to the input as bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.data
    }

    /// Gets the input as individual elements, one per line.
    pub fn lines<T>(&self) -> Vec<T>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Debug,
    {
        nom::multi::many0(nom::sequence::terminated(
            // TODO: this should respect all types of line endings
            nom::combinator::map(
                nom::bytes::complete::take_till1(|x: char| x == '\n'),
                |x: &str| x.parse::<T>().unwrap(),
            ),
            nom::character::complete::line_ending::<_, nom::error::VerboseError<_>>,
        ))(self.str())
        .finish()
        .unwrap()
        .1
    }

    /// Gets the input as a collection of elements, with each group of elements separated by a newline.
    pub fn groups<T>(&self) -> Vec<Vec<T>>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Debug,
    {
        nom::multi::separated_list0(
            nom::character::complete::line_ending::<_, nom::error::VerboseError<_>>,
            nom::multi::many1(nom::sequence::terminated(
                //nom::combinator::map(nom::character::complete::not_line_ending, |x: &str| x.to_string()),
                // TODO: this should respect all types of line endings
                nom::combinator::map(
                    nom::bytes::complete::take_till1(|x: char| x == '\n'),
                    |x: &str| x.parse::<T>().unwrap(),
                ),
                nom::character::complete::line_ending,
            )),
        )(self.str())
        .finish()
        .unwrap()
        .1
    }

    /// Gets the input as a sequence of pairs (one pair per line, pairs separated by whitespace).
    pub fn pairs<T, U>(&self) -> Vec<(T, U)>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Debug,
        U: std::str::FromStr,
        <U as std::str::FromStr>::Err: std::fmt::Debug,
    {
        nom::sequence::terminated(
            nom::multi::many0(nom::sequence::terminated(
                nom::sequence::separated_pair(
                    nom::combinator::map(
                        nom::bytes::complete::take_till1(|x: char| x.is_whitespace()),
                        |x: &str| x.parse::<T>().unwrap(),
                    ),
                    nom::character::complete::space1::<_, nom::error::VerboseError<_>>,
                    nom::combinator::map(
                        nom::bytes::complete::take_till1(|x: char| x.is_whitespace()),
                        |x: &str| x.parse::<U>().unwrap(),
                    ),
                ),
                nom::character::complete::line_ending,
            )),
            nom::combinator::eof,
        )(self.str())
        .finish()
        .unwrap()
        .1
    }

    pub fn grid<T>(&self) -> HashMap<(usize, usize), T>
    where
        T: From<char>,
    {
        let vecvec: Vec<Vec<char>> =
            nom::combinator::all_consuming(nom::multi::many0(nom::sequence::terminated(
                nom::multi::many0(nom::sequence::preceded(
                    nom::combinator::not(nom::character::complete::line_ending),
                    nom::character::complete::anychar::<_, nom::error::VerboseError<_>>,
                )),
                nom::character::complete::line_ending,
            )))(self.str())
            .finish()
            .unwrap()
            .1;
        vecvec
            .into_iter()
            .enumerate()
            .flat_map(|(x, vec)| {
                vec.into_iter()
                    .enumerate()
                    .map(move |(y, c)| ((x, y), c.into()))
            })
            .collect()
    }
}
