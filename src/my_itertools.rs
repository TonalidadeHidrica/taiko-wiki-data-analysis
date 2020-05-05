// Why should I do this???
#[allow(unused_imports)]
use itertools::Itertools;

pub trait MyItertools: Iterator {
    fn split_first(mut self) -> (Option<Self::Item>, Self)
    where
        Self: Sized,
    {
        (self.next(), self)
    }
}

impl<T: ?Sized> MyItertools for T where T: Iterator {}

#[test]
fn test_take_first() {
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
