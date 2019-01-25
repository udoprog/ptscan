use std::error;

fn main() -> Result<(), Box<error::Error>> {
    lalrpop::process_root()?;
    Ok(())
}
