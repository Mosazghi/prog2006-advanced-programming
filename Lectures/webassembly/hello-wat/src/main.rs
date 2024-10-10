
use std::env;
use std::fs;
use std::mem;
use std::ops::Range;
use wasmer::{Store, Function, FunctionEnv, FunctionEnvMut, Module, Instance, imports, Memory};


struct Env {
    memory: Option<Memory>,
}

// Define our own host function to print a string
fn print_string(mut env: FunctionEnvMut<Env>, string_ptr: i32, string_len: i32) {
    let (env_data, store) = env.data_and_store_mut();
    let memory_view = env_data.memory.as_ref().unwrap().view(&store);
    // https://docs.rs/wasmer/latest/wasmer/struct.MemoryView.html#method.copy_to_vec
    let memory_buffer = memory_view.copy_range_to_vec(Range { start: 0, end: 1024 }).unwrap();

    let extracted_slice = &memory_buffer[string_ptr as usize..(string_ptr + string_len) as usize];
    let string = std::str::from_utf8(extracted_slice).unwrap();
    println!("{}", string);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut store = Store::default();
    
    let env = FunctionEnv::new(&mut store, Env { memory: None, });

    // Create a host function
    let print_string_function = Function::new_typed_with_env(&mut store, &env, print_string);

    // Create an import object with the host function
    let import_object = imports! {
        "env" => {
            "print_string" => print_string_function,
        },
    };

    // Get the command-line arguments
    let args: Vec<String> = env::args().collect();

    // Check if an argument containing the path to the .wasm module is provided
    if args.len() < 3 {
        eprintln!("Usage: {} <path_to_wasm_module> <void_function_to_call>", args[0]);
        std::process::exit(1);
    }

    // Get the path to the .wasm module from the command-line arguments
    let wasm_path = &args[1];
    let function_name = &args[2];

    // Read the .wasm module bytes from the file
    let wasm_bytes = match fs::read(wasm_path) {
        Ok(bytes) => bytes,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };
    let module = Module::new(&store, wasm_bytes).unwrap();
    let instance = Instance::new(&mut store, &module, &import_object)?;

    let memory = instance.exports.get_memory("memory").unwrap().clone();
    env.as_mut(&mut store).memory = Some(memory);

    let entry = instance.exports.get_function(function_name)?;
    entry.call(&mut store, &[])?;

    Ok(())
}