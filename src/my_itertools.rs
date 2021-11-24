pub trait MyItertools: Iterator {
    fn split_first(mut self) -> (Option<Self::Item>, Self)
    where
        Self: Sized,
    {
        (self.next(), self)
    }

    #[inline]
    fn take_until<P>(self, predicate: P) -> TakeUntil<Self, P>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool,
    {
        TakeUntil::new(self, predicate)
    }
}

impl<T: ?Sized> MyItertools for T where T: Iterator {}

#[must_use = "iterators are lazy and do nothing unless consumed"]
#[derive(Clone)]
pub struct TakeUntil<I, P> {
    iter: I,
    flag: bool,
    predicate: P,
}

impl<I, P> TakeUntil<I, P> {
    fn new(iter: I, predicate: P) -> TakeUntil<I, P> {
        TakeUntil {
            iter,
            flag: false,
            predicate,
        }
    }
}

impl<I: Iterator, P> Iterator for TakeUntil<I, P>
where
    P: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        if self.flag {
            None
        } else {
            let x = self.iter.next()?;
            self.flag = (self.predicate)(&x);
            Some(x)
        }
    }
}

#[test]
fn test_take_first() {
    use itertools::Itertools;

    let vec = vec![3, 4, 5];
    let (first, remain) = vec.iter().split_first();
    assert_eq!(first, Some(&3));
    assert_eq!(remain.collect_vec(), vec![&4, &5]);

    let vec = vec![6];
    let (first, remain) = vec.iter().split_first();
    assert_eq!(first, Some(&6));
    assert!(remain.collect_vec().is_empty());

    let vec: Vec<i32> = vec![];
    let (first, remain) = vec.iter().split_first();
    assert_eq!(first, None);
    assert!(remain.collect_vec().is_empty());
}
