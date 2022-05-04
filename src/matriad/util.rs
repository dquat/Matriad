use std::fmt::{ Debug, Display, Formatter };
use std::slice::Iter;

/// A small implementation of an array (stack) based vector.
/// It is about 6x faster than Vec, when the size is known.
/// Overflow is not supported, but underflow is allowed
/// unused in the project, for now
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SArr<T, const SIZE: usize> {
    /// This is an array of constant size that holds the data in the array
    data : [T; SIZE],
    /// The length of the array that is essentially "occupied" by valid elements
    len  : usize,
}

impl<T: Debug, const SIZE: usize> Display for SArr<T, SIZE> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.data[..self.len])
    }
}

impl<T, const SIZE: usize> SArr<T, SIZE> {
    pub fn new() -> Self
        where T: Copy + Default {
        Self {
            data : [T::default(); SIZE],
            len  : 0,
        }
    }

    pub fn from_value(data: T) -> Self
        where T: Copy {
        Self {
            data : [data; SIZE],
            len  : 0,
        }
    }

    pub fn from_slice(array: [T; SIZE]) -> Self {
        Self {
            data : array,
            len  : 0,
        }
    }

    pub fn from_raw_parts(array: [T; SIZE], location: usize) -> Self {
        Self {
            data : array,
            len  : location,
        }
    }

    pub fn get_unchecked(&self, loc: usize) -> &T {
        self.data.get(loc).unwrap()
    }

    pub fn get(&self, loc: usize) -> Option<&T> {
        self.data.get(loc)
    }

    pub fn get_mut(&mut self, loc: usize) -> Option<&mut T> {
        self.data.get_mut(loc)
    }

    pub fn get_mut_unchecked(&mut self, loc: usize) -> &mut T {
        self.data.get_mut(loc).unwrap()
    }

    pub fn push(&mut self, value: T) {
        let data = self.get_mut(self.len).expect("Cannot push, the array is full!");
        *data = value;
        self.len += 1;
    }

    pub fn try_push(&mut self, value: T) -> Result<(), ()> {
        let data = self.get_mut(self.len);
        if let Some(data) = data {
            *data = value;
            self.len += 1;
            return Ok(());
        }
        Err(())
    }

    pub fn pop(&mut self) -> Option<T>
        where T: Default {
        if self.len == 0 { return None; }
        self.len -= 1;
        Some(unsafe { self.data.as_ptr().add(self.len).read() })
    }

    pub fn remove_len(&mut self, length: usize) {
        if self.len > length {
            self.len -= length;
        } else {
            self.len = 0;
        }
    }

    pub fn extend_copy(&mut self, iter: Iter<T>)
        where T: Copy {
        iter.copied().for_each(|value| { self.try_push(value).ok(); });
    }

    pub fn extend_clone(&mut self, iter: Iter<T>)
        where T: Clone {
        iter.cloned().for_each(|value| { self.try_push(value).ok(); });
    }

    pub fn extend_slice_copy(&mut self, slice: &[T])
        where T: Copy {
        let len = slice.len();
        for (i, item) in slice.iter().enumerate() {
            if self.len + i < SIZE {
                self.data[self.len + i] = *item;
            }
        }
        self.len += len;
        if self.len > SIZE {
            self.len = SIZE;
        }
    }

    pub fn extend_slice_clone(&mut self, slice: &[T])
        where T: Clone {
        let len = slice.len();
        for (i, item) in slice.iter().enumerate() {
            if self.len + i < SIZE {
                self.data[self.len + i] = item.clone();
            }
        }
        self.len += len;
        if self.len > SIZE {
            self.len = SIZE;
        }
    }

    pub fn clear(&mut self) { self.len = 0; }

    pub fn len(&self) -> usize { self.len }
}

/// A simple span of lines, or a start and end position
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Span {
    /// The start location that the span references in the source
    pub start : usize,
    /// The end location that the span references in the source
    pub end   : usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self  {
        Self { start, end }
    }

    #[inline]
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }

    pub fn from_range(range: std::ops::Range<usize>) -> Self {
        Self { start: range.start, end: range.end }
    }

    pub fn len(&self) -> usize {
        if self.end == self.start {
            0
        } else if self.end >= self.start {
            self.end   - self.start
        } else {
            self.start - self.end
        }
    }

    #[inline]
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.start, &mut self.end);
    }

    #[inline]
    pub fn to_src<'a>(&self, src: &'a str) -> &'a str {
        &src[self.range()]
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}