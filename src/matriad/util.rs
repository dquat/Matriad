use std::fmt::{
    Debug,
    Display,
    Formatter
};
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
    /// Create a new instance of the array with a default value of `<T>`
    /// Requires that `T` has the `Copy` and `Default` traits
    pub fn new() -> Self
        where T: Copy + Default {
        Self {
            data : [T::default(); SIZE],
            len  : 0,
        }
    }

    /// Create an array from a value. Cop
    pub fn from_value(data: T) -> Self
        where T: Copy {
        Self {
            data : [data; SIZE],
            len  : 0,
        }
    }

    /// Create an array from a slice
    pub fn from_slice(array: [T; SIZE]) -> Self {
        Self {
            data : array,
            len  : 0,
        }
    }

    /// Create an array from raw parts, i.e. length and the slice
    pub fn from_raw_parts(array: [T; SIZE], mut location: usize) -> Self {
        if location > SIZE { location = SIZE; }
        Self {
            data : array,
            len  : location,
        }
    }

    /// Get a reference to a value at an index. If the index is too high, the code will panic
    pub fn get_unchecked(&self, loc: usize) -> &T {
        self.data.get(loc).unwrap()
    }

    /// Get a reference to a value at an index
    pub fn get(&self, loc: usize) -> Option<&T> {
        self.data.get(loc)
    }

    /// Get a mutable reference to a value at an index
    pub fn get_mut(&mut self, loc: usize) -> Option<&mut T> {
        self.data.get_mut(loc)
    }

    /// Get a mutable reference to a value at an index. If the index is too high, the code will
    /// panic
    pub fn get_mut_unchecked(&mut self, loc: usize) -> &mut T {
        self.data.get_mut(loc).unwrap()
    }

    /// Push array with checks
    /// If the array is full, the program will panic
    pub fn push(&mut self, value: T) {
        let data =
            self
                .get_mut(self.len)
                .expect("Cannot push, the array is full!");
        *data = value;
        self.len += 1;
    }

    /// Try pushing a value into the array
    /// If the array is full, the items cannot be pushed, so an error is returned
    pub fn try_push(&mut self, value: T) -> Result<(), ()> {
        let data = self.get_mut(self.len);
        if let Some(data) = data {
            *data = value;
            self.len += 1;
            return Ok(());
        }
        Err(())
    }

    /// Pop an element out of the array and return it
    pub fn pop(&mut self) -> Option<T>
        where T: Default {
        if self.len == 0 { return None; }
        self.len -= 1;
        // Safety: Well we know that the length can't be less than 0, since we just checked above
        // and the length, will never become greater than the size since none of the other code
        // allows for the user to do so
        Some(unsafe { self.data.as_ptr().add(self.len).read() })
    }

    /// Remove a specific length off
    /// If this is greater than the max size of the array, the length will become 0
    pub fn remove_len(&mut self, length: usize) {
        if self.len > length {
            self.len -= length;
        } else {
            self.len = 0;
        }
    }

    /// Extend the array by copying the element with an iterator
    pub fn extend_copy(&mut self, iter: Iter<T>)
        where T: Copy {
        iter.copied().for_each(|value| { self.try_push(value).ok(); });
    }

    /// Extend the array by cloning the element with an iterator
    pub fn extend_clone(&mut self, iter: Iter<T>)
        where T: Clone {
        iter.cloned().for_each(|value| { self.try_push(value).ok(); });
    }

    /// Extend the array by copying the elements
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

    /// Extend the array by cloning the elements
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

    /// Clear all values from the array
    pub fn clear(&mut self) { self.len = 0; }

    /// Get the length of the array
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
    /// Create a new span
    pub fn new(start: usize, end: usize) -> Self  {
        Self { start, end }
    }

    /// Create a new span from single value
    pub fn single(val: usize) -> Self  {
        Self {
            start : val,
            end   : val,
        }
    }

    /// Convert the span to a [`Range<usize>`](core::ops::Range)
    #[inline]
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }

    /// Create a span from a [`Range<usize>`](core::ops::Range)
    pub fn from_range(range: std::ops::Range<usize>) -> Self {
        Self {
            start : range.start,
            end   : range.end
        }
    }

    /// Find out the distance covered by the span, i.e. length of the span
    pub fn len(&self) -> usize {
        if self.end == self.start {
            0
        } else if self.end >= self.start {
            self.end   - self.start
        } else {
            self.start - self.end
        }
    }

    /// Swap the values of the span
    #[inline]
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.start, &mut self.end);
    }

    /// Show where the span lies, given a source string
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