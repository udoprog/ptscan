use std::error;

fn main() -> Result<(), Box<dyn error::Error>> {
    lalrpop::process_root()?;
    Ok(())
}
