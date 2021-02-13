mod Cargo;

/* fn get_version() -> &'static str {
    // return is only used for early returns
    // match env!("CARGO_PKG_VERSION").parse::<u16>() {
    //     Ok(uint) => uint,
    //     // not quite ideal, but instead of cleanly exiting with Err(e), might as
    //     //   well just segfault at this point, or at least it should be
    //     result => result.unwrap(),
    // }
} */

fn usage() {
    println!("No usage instructions...");
    println!("{} {}", Cargo::PKG_NAME, Cargo::VERSION);
}

fn parse_args() {
    let args: Vec<String> = std::env::args().collect();
}

fn main() {
    println!("Hello World!");
    usage();
}
