use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::atomic::{self, AtomicU64},
};

/// A set of [`Tag`]s, optimized for small sizes.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tags {
    tags: Vec<Tag>,
}

impl Tags {
    pub const fn new() -> Self {
        Self { tags: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.tags.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    /// Check if `other` is a subset of `self`.
    pub fn is_subset(&self, other: &Self) -> bool {
        if other.len() > self.len() {
            return false;
        }

        let mut self_iter = self.tags.iter();
        let mut other_iter = other.tags.iter().peekable();

        loop {
            let Some(other_tag) = other_iter.peek() else {
                return true;
            };

            let Some(self_tag) = self_iter.next() else {
                return false;
            };

            if self_tag == *other_tag {
                other_iter.next();
            } else {
                continue;
            }
        }
    }

    pub fn contains(&self, tag: Tag) -> bool {
        self.tags.binary_search(&tag).is_ok()
    }

    pub fn insert(&mut self, tag: Tag) {
        if let Err(pos) = self.tags.binary_search(&tag) {
            self.tags.insert(pos, tag);
        }
    }

    pub fn remove(&mut self, tag: &Tag) {
        if let Ok(pos) = self.tags.binary_search(tag) {
            self.tags.remove(pos);
        }
    }

    pub fn union(&mut self, other: &Self) {
        self.tags.extend_from_slice(&other.tags);
        self.tags.sort_unstable();
        self.tags.dedup();
    }

    pub fn clear(&mut self) {
        self.tags.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = Tag> {
        self.tags.iter().copied()
    }
}

impl From<Tag> for Tags {
    fn from(tag: Tag) -> Self {
        let mut tags = Self::new();
        tags.insert(tag);
        tags
    }
}

/// A tag used to identify traits.
#[derive(Clone, Copy, Debug)]
pub struct Tag {
    data: u64,
    name: &'static str,
}

impl Tag {
    pub const INT: Tag = Tag::new(0x1, "int");
    pub const FLOAT: Tag = Tag::new(0x2, "float");
    pub const STR: Tag = Tag::new(0x3, "str");
    pub const NONE: Tag = Tag::new(0x4, "none");
    pub const TRUE: Tag = Tag::new(0x5, "true");
    pub const FALSE: Tag = Tag::new(0x6, "false");

    /// The bitmask for the reserved tags.
    pub const RESERVED_MASK: u64 = 0xffff_0000_0000_0000;

    /// The maximum value for a generated tag.
    pub const MAX_DATA: u64 = 0x0000_ffff_ffff_ffff;

    /// Create a new tag with the given data and name.
    pub const fn new(data: u16, name: &'static str) -> Self {
        Self {
            data: data as u64 + (1 << 48),
            name,
        }
    }

    /// Create a new tag with generated data and the given name.
    pub fn generate(name: &'static str) -> Self {
        static NEXT_TAG: AtomicU64 = AtomicU64::new(0x0);

        let data = NEXT_TAG.fetch_add(1, atomic::Ordering::Relaxed);
        assert!(data <= Self::MAX_DATA, "Tag data overflow");

        Self { data, name }
    }

    pub const fn data(self) -> u64 {
        self.data
    }

    pub const fn name(self) -> &'static str {
        self.name
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.name)
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Eq for Tag {}

impl PartialOrd for Tag {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Tag {
    fn cmp(&self, other: &Self) -> Ordering {
        self.data.cmp(&other.data)
    }
}

impl Hash for Tag {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TAG_A: Tag = Tag::new(0x1, "A");
    const TAG_B: Tag = Tag::new(0x2, "B");
    const TAG_C: Tag = Tag::new(0x3, "C");
    const TAG_D: Tag = Tag::new(0x4, "D");

    #[test]
    fn test_tag_equality() {
        assert_eq!(TAG_A, TAG_A);
        assert_ne!(TAG_A, TAG_B);

        assert_eq!(Tag::new(0x1, "A"), Tag::new(0x1, "B"));
        assert_ne!(Tag::new(0x1, "A"), Tag::new(0x2, "A"));
    }

    #[test]
    fn test_tag_ordering() {
        assert!(TAG_A < TAG_B);
        assert!(TAG_B > TAG_A);

        assert!(Tag::new(0x1, "A") < Tag::new(0x2, "B"));
        assert!(Tag::new(0x2, "B") > Tag::new(0x1, "A"));
    }

    #[test]
    fn test_tag_reserved_data() {
        let lower = Tag::new(u16::MIN, "");
        let upper = Tag::new(u16::MAX, "");

        assert_ne!(lower.data() & Tag::RESERVED_MASK, 0);
        assert_ne!(upper.data() & Tag::RESERVED_MASK, 0);

        let outside = Tag::generate("");
        assert_eq!(outside.data() & Tag::RESERVED_MASK, 0);
    }

    #[test]
    fn test_tag_generate() {
        let a = Tag::generate("");
        let b = Tag::generate("");

        assert_ne!(a.data(), b.data());
    }

    #[test]
    fn test_tags_insert() {
        let mut tags = Tags::new();
        assert!(tags.is_empty());

        tags.insert(TAG_A);
        assert_eq!(tags.len(), 1);
        assert!(tags.contains(TAG_A));
        assert!(!tags.contains(TAG_B));

        tags.insert(TAG_B);
        assert_eq!(tags.len(), 2);
        assert!(tags.contains(TAG_A));
        assert!(tags.contains(TAG_B));
    }

    #[test]
    fn test_tags_remove() {
        let mut tags = Tags::new();
        tags.insert(TAG_A);
        tags.insert(TAG_B);
        assert_eq!(tags.len(), 2);

        tags.remove(&TAG_A);
        assert_eq!(tags.len(), 1);
        assert!(!tags.contains(TAG_A));
        assert!(tags.contains(TAG_B));

        tags.remove(&TAG_B);
        assert_eq!(tags.len(), 0);
        assert!(!tags.contains(TAG_A));
        assert!(!tags.contains(TAG_B));
    }

    #[test]
    fn test_tags_union() {
        let mut tags_a = Tags::new();
        tags_a.insert(TAG_A);
        tags_a.insert(TAG_B);

        let mut tags_b = Tags::new();
        tags_b.insert(TAG_C);
        tags_b.insert(TAG_D);

        assert_eq!(tags_a.len(), 2);
        assert_eq!(tags_b.len(), 2);

        tags_a.union(&tags_b);
        assert_eq!(tags_a.len(), 4);
        assert!(tags_a.contains(TAG_A));
        assert!(tags_a.contains(TAG_B));
        assert!(tags_a.contains(TAG_C));
        assert!(tags_a.contains(TAG_D));
    }

    #[test]
    fn test_tags_is_subset() {
        let mut tags_a = Tags::new();
        tags_a.insert(TAG_A);
        tags_a.insert(TAG_B);

        let mut tags_b = Tags::new();
        tags_b.insert(TAG_A);
        tags_b.insert(TAG_B);
        tags_b.insert(TAG_C);

        assert!(tags_b.is_subset(&tags_a));
        assert!(!tags_a.is_subset(&tags_b));

        tags_a.insert(TAG_C);
        assert!(tags_a.is_subset(&tags_b));
        assert!(tags_b.is_subset(&tags_a));

        tags_a.insert(TAG_D);
        assert!(tags_a.is_subset(&tags_b));
        assert!(!tags_b.is_subset(&tags_a));
    }
}
