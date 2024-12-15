use std::iter::IntoIterator;

// A custom struct:
struct Droppable {
    name: &'static str,
}

// A custom implementation of the Drop trait for the struct:
impl Drop for Droppable {
    // Rust calls automatically `drop()` for each field of a struct. A custom
    // implementation of the Drop trait needs only to dealocacte resources
    // introduced by the struct. Hence this `drop()` implementation does not
    // actually deallocate anything:
    fn drop(&mut self) {
        println!("> Dropping {}", self.name);
    }
}

fn custom_drop_example() {
    let _droppable = Droppable { name: "test value" };

    // Won't compile as Rust does not allow explicit calls to drop:
    // _droppable.drop();
} // `Droppable::drop()` is called automatically here.

fn file_drop_example() {
    // Open a directory:
    let dir_path = std::env::current_dir().unwrap();

    // Get files present in the directory:
    let dir_filepaths = std::fs::read_dir(dir_path).unwrap();

    // Open each file:
    for filepath in dir_filepaths {
        let _file = std::fs::File::open(filepath.unwrap().path()).unwrap();
        // There is no `close()` in Rust.
    } // Dropping `_file` closes the opened file.
}

fn iterate_taking_ownership_of_elements_example() {
    let v1 = vec![Droppable { name: "a1" }, Droppable { name: "b1" }];
    let v2 = vec![Droppable { name: "a2" }, Droppable { name: "b2" }];
    let mut v3 = vec![Droppable { name: "a3" }, Droppable { name: "b3" }];

    for droppable in v1 {
        // In every iteration the variable `droppable` takes ownership of a vector's
        // element, and the element is dropped at the end of the iteration.
        println!("{} will be dropped", droppable.name);
    }

    // The above loop implicitly calls `IntoIterator::into_iter()` on `v1`, like this:
    for droppable in v2.into_iter() {
        println!("{} will be dropped", droppable.name);
    }
    // Notice that `IntoIterator::into_iter()` takes its argument by value.

    // Won't compile, because values of `v1` and `v2` were moved.
    // println!("v1.len() == {}", v1.len());
    // println!("v2.len() == {}", v2.len());

    // To conveniently iterate and take ownership of a vector's elements without taking
    // ownership of the vector:
    for droppable in v3.drain(..) {
        println!("{} will be dropped", droppable.name);
    }

    println!("v3.len() == {}", v3.len());
}

fn main() {
    custom_drop_example();
    file_drop_example();
    iterate_taking_ownership_of_elements_example();
}
