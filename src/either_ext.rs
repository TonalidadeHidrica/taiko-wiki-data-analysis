use either::Either;

pub trait EitherExt<L, R> {
    fn into_common<T>(self) -> T
    where
        T: From<L> + From<R>;
}
impl<L, R> EitherExt<L, R> for Either<L, R> {
    fn into_common<T>(self) -> T
    where
        T: From<L> + From<R>,
    {
        match self {
            Either::Left(l) => l.into(),
            Either::Right(r) => r.into(),
        }
    }
}

pub fn into_common_2<L, RL, RR, T>(e: Either<L, Either<RL, RR>>) -> T
where
    T: From<L> + From<RL> + From<RR>,
{
    e.map_right::<_, T>(EitherExt::into_common).into_common()
}
