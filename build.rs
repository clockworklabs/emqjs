fn main() {
    if cfg!(target_os = "wasi") {
        println!("cargo:rerun-if-changed=temp.o");
        println!("cargo:rustc-link-arg=temp.o");
    }
}
