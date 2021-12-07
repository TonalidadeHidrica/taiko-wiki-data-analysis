use std::{
    mem::replace,
    ops::{Index, Range},
};

pub trait MatchLike {
    fn start_pos(&self) -> usize;
    fn end_pos(&self) -> usize;
}
impl MatchLike for regex::Match<'_> {
    fn start_pos(&self) -> usize {
        self.start()
    }

    fn end_pos(&self) -> usize {
        self.end()
    }
}
impl MatchLike for regex::Captures<'_> {
    fn start_pos(&self) -> usize {
        self.get(0).unwrap().start()
    }

    fn end_pos(&self) -> usize {
        self.get(0).unwrap().end()
    }
}
impl MatchLike for pcre::Match<'_> {
    fn start_pos(&self) -> usize {
        self.group_start(0)
    }

    fn end_pos(&self) -> usize {
        self.group_end(0)
    }
}
impl MatchLike for Range<usize> {
    fn start_pos(&self) -> usize {
        self.start
    }
    fn end_pos(&self) -> usize {
        self.end
    }
}

impl<T> MatchLike for &T
where
    T: MatchLike,
{
    fn start_pos(&self) -> usize {
        T::start_pos(self)
    }
    fn end_pos(&self) -> usize {
        T::end_pos(self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum MatchComponent<M, B> {
    Match(M),
    Between(B),
}
#[derive(Clone, Debug)]
pub struct MatchComponentIterator<S, I, M> {
    str: S,
    iter: I,
    pos: usize,
    len: usize,
    state: Next<M>,
}
#[derive(Clone, Debug)]
enum Next<M> {
    Str(Option<M>),
    Match(M),
    Finished,
    Dummy,
}
impl<S, I, M> Iterator for MatchComponentIterator<S, I, M>
where
    I: Iterator<Item = M>,
    M: MatchLike,
    S: IndexOwned<Range<usize>, Output=S> + Copy,
{
    type Item = MatchComponent<I::Item, S>;

    fn next(&mut self) -> Option<Self::Item> {
        match replace(&mut self.state, Next::Dummy) {
            Next::Str(next) => {
                let end = next.as_ref().map_or(self.len, |x| x.start_pos());
                self.state = match next {
                    Some(next) => Next::Match(next),
                    None => Next::Finished,
                };
                if self.pos < end {
                    Some(MatchComponent::Between(self.str.index_owned(self.pos..end)))
                } else {
                    self.next()
                }
            }
            Next::Match(next) => {
                self.state = Next::Str(self.iter.next());
                self.pos = next.end_pos();
                Some(MatchComponent::Match(next))
            }
            Next::Finished => None,
            Next::Dummy => panic!(),
        }
    }
}

pub trait IndexOwned<I> {
    type Output;
    fn index_owned(self, index: I) -> Self::Output;
}

impl <'a, I> IndexOwned<I> for &'a str where str: Index<I, Output=str> {
    type Output = &'a str;

    fn index_owned(self, index: I) -> Self::Output {
        &self[index]
    }
}

pub trait MatchIterator<S, M: MatchLike>: Iterator<Item = M> + Sized {
    fn match_components(mut self, str: S, len: usize) -> MatchComponentIterator<S, Self, M> {
        let next = self.next();
        MatchComponentIterator {
            str,
            iter: self,
            pos: 0,
            state: Next::Str(next),
            len
        }
    }
}

impl<S, I: Iterator> MatchIterator<S, I::Item> for I where I::Item: MatchLike {}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use regex::Regex;

    use super::{
        MatchComponent::{Between, Match as CMatch},
        MatchIterator,
    };

    #[test]
    fn test_match_iterator() {
        let regex = Regex::new("[A-Z]{3}").unwrap();

        let haystack = "hogeABCtesttestDEFghi";
        let matches = regex.find_iter(haystack).collect_vec();
        assert_eq!(matches.len(), 2);
        let mut it = regex.find_iter(haystack).match_components(haystack, haystack.len());
        assert_eq!(it.next(), Some(Between("hoge")));
        assert_eq!(it.next(), Some(CMatch(matches[0])));
        assert_eq!(it.next(), Some(Between("testtest")));
        assert_eq!(it.next(), Some(CMatch(matches[1])));
        assert_eq!(it.next(), Some(Between("ghi")));
        assert_eq!(it.next(), None);

        let haystack = "ABCtestSERVALcatDEF";
        let matches = regex.find_iter(haystack).collect_vec();
        assert_eq!(matches.len(), 4);
        let mut it = regex.find_iter(haystack).match_components(haystack, haystack.len());
        assert_eq!(it.next(), Some(CMatch(matches[0])));
        assert_eq!(it.next(), Some(Between("test")));
        assert_eq!(it.next(), Some(CMatch(matches[1])));
        assert_eq!(it.next(), Some(CMatch(matches[2])));
        assert_eq!(it.next(), Some(Between("cat")));
        assert_eq!(it.next(), Some(CMatch(matches[3])));
        assert_eq!(it.next(), None);
    }
}
