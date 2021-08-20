#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub mod value;
pub use value::{Endian, Value};
