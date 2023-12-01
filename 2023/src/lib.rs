#[macro_export]
macro_rules! day_function(
    ($day:expr, $input:ident, $body:expr) => (
        pub fn solve($input: advent::Input) -> (String, String) {
            $body
        }
    )
);
