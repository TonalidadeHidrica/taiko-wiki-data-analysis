use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

pub trait RangeSlice<T> {
    fn start_bound_slice(&self) -> Option<&T>;
    fn end_bound_slice(&self) -> Option<&T>;
}

impl <T> RangeSlice<T> for Range<T> {
    fn start_bound_slice(&self) -> Option<&T> {
        Some(&self.start)
    }

    fn end_bound_slice(&self) -> Option<&T> {
        Some(&self.end)
    }
}

impl <T> RangeSlice<T> for Range<&'_ T> {
    fn start_bound_slice(&self) -> Option<&T> {
        Some(self.start)
    }

    fn end_bound_slice(&self) -> Option<&T> {
        Some(self.end)
    }
}

impl <T> RangeSlice<T> for RangeFrom<T> {
    fn start_bound_slice(&self) -> Option<&T> {
        Some(&self.start)
    }

    fn end_bound_slice(&self) -> Option<&T> {
        None
    }
}

impl <T> RangeSlice<T> for RangeFrom<&'_ T> {
    fn start_bound_slice(&self) -> Option<&T> {
        Some(self.start)
    }

    fn end_bound_slice(&self) -> Option<&T> {
        None
    }
}

impl <T> RangeSlice<T> for RangeTo<T> {
    fn start_bound_slice(&self) -> Option<&T> {
        None
    }

    fn end_bound_slice(&self) -> Option<&T> {
        Some(&self.end)
    }
}

impl <T> RangeSlice<T> for RangeTo<&'_ T> {
    fn start_bound_slice(&self) -> Option<&T> {
        None
    }

    fn end_bound_slice(&self) -> Option<&T> {
        Some(self.end)
    }
}

impl <T> RangeSlice<T> for RangeFull {
    fn start_bound_slice(&self) -> Option<&T> {
        None
    }

    fn end_bound_slice(&self) -> Option<&T> {
        None
    }
}
