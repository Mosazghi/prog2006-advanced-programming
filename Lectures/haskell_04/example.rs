
use std::error::Error;

fn get_input() -> Result<String, Box<dyn Error>> {
    // This is a placeholder for a real implementation
    Ok("{'name': Mariusz, 'email': example.com}".to_string())
}

fn parse_input_and_get_email(input: &str) -> Result<String, Box<dyn Error>> {
    // This is a placeholder for a real implementation
    Ok("example.com".to_string())
}

fn validate_email(email: &str) -> Result<String, Box<dyn Error>> {
    // This is a placeholder for a real implementation
    Err("Invalid email".into())
}

fn process_notification(email: &str) -> Result<String, Box<dyn Error>> {
    // This is a placeholder for a real implementation
    Ok("Notification sent".to_string())
}

fn processor() -> Result<(), Box<dyn Error>> {
    let input = get_input()?;
    let email = parse_input_and_get_email(&input)?;
    let email = validate_email(&email)?;
    process_notification(&email)?;
    Ok(())
}

/* Discussion on what and how one can 
compose computations in imperative and
functional programming languages

let x = doA()
let y = doB(&x)
doC(&y)

doC(doB(doA()))

Rust has neat type system that allows us to
compose computations in a functional way

doA()
.and_then(|x| doB(&x))
.and_then(|y| doC(&y))

Often we see this type of composition in
imperative programming languages, based
on the functions that the returning value 
can offer:

container
  .map(|x| x + 1)
  .filt


In Haskell we have those:

<space> - function application

. - function composition

<$> - map for applicative functors

<*> - composition for applicative functor

>>= - bind operator, composition for Monads

foreach
for i in container_variable {
  get element by index
}

for i,v := range container_variable {
  get element by index
}

Functions:
* pure (impure)
* total (partial)


*/