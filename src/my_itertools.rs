use std::{iter::Peekable, marker::PhantomData, ops::Deref};

use itertools::Itertools;

use crate::regex_ext::iter::MatchLike;

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

    // #[inline]
    // fn remove_overlapping<T, J>(self, filter: J) -> RemoveOverlapping<Self, J::IntoIter, J::Item>
    // where
    //     Self: Sized,
    //     J: IntoIterator,
    #[inline]
    fn remove_overlapping<J>(self, filter: J) -> RemoveOverlapping<Self, J::IntoIter, J::Item>
    where
        Self: Sized + Iterator,
        Self::Item: MatchLike,
        J: IntoIterator,
        J::Item: MatchLike,
    {
        RemoveOverlapping {
            it: self,
            filter: filter.into_iter().peekable(),
            _phantom: Default::default(),
        }
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

pub struct RemoveOverlapping<I, J, JT>
where
    J: Iterator,
{
    it: I,
    filter: Peekable<J>,
    _phantom: PhantomData<fn() -> JT>,
}
impl<I, IT, J, JT> Iterator for RemoveOverlapping<I, J, JT>
where
    I: Iterator<Item = IT>,
    IT: MatchLike,
    J: Iterator<Item = JT>,
    // JB: Borrow<JT>,
    JT: MatchLike,
    // T: std::fmt::Debug,
{
    type Item = IT;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.find(|subject| {
            self.filter
                .peeking_find(|next| subject.start_pos() < next.end_pos())
                .map_or(true, |next| subject.end_pos() <= next.start_pos())
        })
    }
}
impl<I, J, JT> Clone for RemoveOverlapping<I, J, JT>
where
    I: Clone,
    J: Iterator,
    Peekable<J>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            it: self.it.clone(),
            filter: self.filter.clone(),
            _phantom: Default::default(),
        }
    }
}

pub trait PeekableExt<T>: Sized {
    // fn peeking_find<P>(&mut self, predicate: P) -> Option<Peeked<T, Self>>
    fn peeking_find<P>(&mut self, predicate: P) -> Option<&T>
    where
        Self: Iterator<Item = T>,
        P: FnMut(&T) -> bool;
}

impl<T, I> PeekableExt<T> for Peekable<I>
where
    I: Iterator<Item = T>,
{
    // fn peeking_find<P>(&mut self, mut predicate: P) -> Option<Peeked<T, Self>>
    fn peeking_find<P>(&mut self, mut predicate: P) -> Option<&T>
    where
        Self: Iterator<Item = T>,
        P: FnMut(&T) -> bool,
    {
        self.peeking_take_while(|x| !predicate(x)).last();
        // self.peek().map(|item| Peeked { item, iterator: self })
        self.peek()
    }
}

pub struct Peeked<'a, T, I> {
    item: &'a T,
    iterator: &'a mut I,
}
impl<'a, T, I> Peeked<'a, T, I>
where
    I: Iterator<Item = T>,
{
    pub fn pop(this: Self) -> T {
        this.iterator.next().unwrap()
    }
}
impl<'a, T, I> Deref for Peeked<'a, T, I> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.item
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::my_itertools::{MyItertools, PeekableExt};

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

    #[test]
    fn test_remove_overlapping() {
        let v = vec![0..4, 13..16, 21..25, 30..31, 37..39, 42..43, 50..55];
        let u = vec![10..14, 23..28, 28..33, 33..37, 43..50];
        let res = v
            .clone()
            .into_iter()
            .remove_overlapping(u.iter())
            .collect_vec();
        let expect = vec![0..4, 37..39, 42..43, 50..55];
        assert_eq!(&res, &expect);

        let res = v.into_iter().remove_overlapping(u).collect_vec();
        assert_eq!(&res, &expect);
    }

    #[test]
    fn test_peeking_find() {
        let mut it = vec![3, 1, 4, 7, 5, 9, 2, 6].into_iter().peekable();
        assert_eq!(it.peeking_find(|x| x % 2 == 0).map(|x| &*x), Some(&4));
        assert_eq!(it.peek(), Some(&4));
        assert_eq!(it.peeking_find(|x| x % 2 == 0).map(|x| &*x), Some(&4));
        assert_eq!(it.peek(), Some(&4));
        assert_eq!(it.next(), Some(4));
        assert_eq!(it.peek(), Some(&7));
    }
}
