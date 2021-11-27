pub fn chmin<T>(input: &mut T, limit: T) -> bool
where
    T: PartialOrd,
{
    if *input > limit {
        *input = limit;
        true
    } else {
        false
    }
}

pub fn chmax<T>(input: &mut T, limit: T) -> bool
where
    T: PartialOrd,
{
    if *input < limit {
        *input = limit;
        true
    } else {
        false
    }
}
