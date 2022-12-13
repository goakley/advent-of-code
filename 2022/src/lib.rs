use nom::Finish;
use std::io::Read;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Point(pub usize, pub usize);

impl Point {
    pub fn within(&self, min: &Point, max: &Point) -> bool {
        let Point(xmin, ymin) = min;
        let Point(xmax, ymax) = max;
        let Point(x, y) = self;
        x >= xmin && x <= xmax && y >= ymin && y <= ymax
    }

    pub fn adjacent(&self, max: &Point) -> Vec<Point> {
        let &Point(x, y) = self;
        let mut v = Vec::with_capacity(4);
        if x > 0 {
            v.push(Point(x - 1, y));
        }
        if y > 0 {
            v.push(Point(x, y - 1));
        }
        if x < max.0 {
            v.push(Point(x + 1, y));
        }
        if y < max.0 {
            v.push(Point(x, y + 1));
        }
        v
    }
}

impl From<(usize, usize)> for Point {
    fn from(p: (usize, usize)) -> Self {
        Point(p.0, p.1)
    }
}

#[derive(Clone)]
pub struct Grid<T>(Vec<Vec<T>>);

impl<T> std::ops::Index<Point> for Grid<T> {
    type Output = T;
    fn index(&self, xy: Point) -> &Self::Output {
        &self.0[xy.0][xy.1]
    }
}

impl<T> std::ops::IndexMut<Point> for Grid<T> {
    fn index_mut(&mut self, xy: Point) -> &mut Self::Output {
        let a = &mut self.0[xy.0];
        &mut a[xy.1]
    }
}

impl<T> From<Vec<Vec<T>>> for Grid<T> {
    fn from(v: Vec<Vec<T>>) -> Self {
        Grid(v)
    }
}

impl<T> Grid<T> {
    pub fn size(&self) -> Point {
        Point(self.0.len(), self.0[0].len())
    }

    pub fn max(&self) -> Point {
        Point(self.0.len() - 1, self.0[0].len() - 1)
    }

    pub fn border_size(&self) -> u64 {
        (self.0.len() as u64) * 2 + ((self.0[0].len() as u64) - 2) * 2
    }

    pub fn cardinal_trees(&self, xy: &Point) -> Vec<Vec<T>>
    where
        T: Copy,
    {
        let x = xy.0;
        let y = xy.1;
        vec![
            (0..x).map(|a| Point(a, y)).rev().collect(),
            (x + 1..self.0.len()).map(|a| Point(a, y)).collect(),
            (0..y).map(|a| Point(x, a)).rev().collect(),
            (y + 1..self.0[0].len()).map(|a| Point(x, a)).collect(),
        ]
        .into_iter()
        .map(|is: Vec<Point>| is.into_iter().map(|i| self[i]).collect())
        .collect()
    }

    pub fn map<B, F>(&self, f: F) -> Grid<B>
    where
        F: Fn(&Point, &T) -> B,
    {
        let mut result: Vec<Vec<B>> = Vec::with_capacity(self.0.len());
        for (x, vec) in self.0.iter().enumerate() {
            let mut intermediate = Vec::with_capacity(self.0[x].len());
            for (y, v) in vec.iter().enumerate() {
                intermediate.push(f(&Point(x, y), v));
            }
            result.push(intermediate);
        }
        Grid(result)
    }

    pub fn iter(&self) -> GridIter<'_, T> {
        GridIter(self, Point(0, 0))
    }
}

impl<T> IntoIterator for Grid<T> {
    type Item = (Point, T);
    type IntoIter = GridIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        GridIterator(
            self.0
                .into_iter()
                .enumerate()
                .flat_map(|(x, v)| {
                    v.into_iter()
                        .enumerate()
                        .map(move |(y, t)| (Point(x, y), t))
                })
                .rev()
                .collect(),
        )
    }
}

pub struct GridIter<'a, T: 'a>(&'a Grid<T>, Point);

impl<'a, T> Iterator for GridIter<'a, T> {
    type Item = (Point, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        let Point(mut x, mut y) = self.1;
        let Point(xmax, ymax) = self.0.max();
        if x > xmax || y > ymax {
            return None;
        }
        let result = (Point(x, y), &self.0[Point(x, y)]);
        y += 1;
        if y > ymax {
            x += 1;
            y = 0;
        }
        self.1 = Point(x, y);
        Some(result)
    }
}

pub struct GridIterator<T>(Vec<(Point, T)>);

impl<T> Iterator for GridIterator<T> {
    type Item = (Point, T);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop()
    }
}

/// An input (example or real) for one day of advent.
pub struct Input {
    data: Vec<u8>,
}

impl Input {
    /// Reads the input for a given day.
    ///
    /// This method will attempt to read from stdin.
    /// If there is no data on stdin, it will instead read the real input from the Advent of Code website (assuming you're logged in).
    pub fn read(day: i64) -> Self {
        let shelf = Self::read_stdin();
        if shelf.data.is_empty() {
            Self::read_day(day)
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
    pub fn read_day(day: i64) -> Self {
        let path = format!("input/{:?}", day);
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
                        .get(format!("https://adventofcode.com/2022/day/{:?}/input", day))
                        .send()
                        .and_then(|r| r.error_for_status())
                        .unwrap()
                        .bytes()
                        .unwrap()
                        .into();
                    std::fs::write(path, &data).unwrap();
                    Input { data }
                }
                _ => Err(e).unwrap(),
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

    /// Get the input as individual elements, one per line.
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
}

#[macro_export]
macro_rules! day_function(
    ($day:expr, $input:ident, $body:expr) => (
        pub fn solve($input: advent_2022::Input) -> (String, String) {
            $body
        }
    )
);
