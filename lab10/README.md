# Lab 10: Hello JSON in Rust

* Deadline: Friday, 22nd of March, 16:00
* Final deadline: tba
* Score: 4 for Rust, 5 for Haskell
* Difficulty: intermediate
* Language: Rust or Haskell
* ChatGPT: **use it**. Ask ChatGPT for explanations and for code.
* Co-pilot: **use it**. Get help from ChatGPT implementing the core functionality.
* Coding: Use Rust book and Rust documentation. Use the suggested external crates.
* Make the code nice, modular and follow the best practices as much as possible.


# Haskell vs. Rust

This lab is designed to be done as "points scoring lab for Rust oblig".
Note however, those that want to include it into their portfolio are encouraged
to (re-)implement it in Haskell, to get the feel of strongly typed facilities
that Haskell offers for web programming.



# Hello JSON in Rust

The task is to build a simple webserver serving some simple API points, and handling JSON input/output.

## The specs 

Create a simple concurrent web server. This exercise will allow you to practice working with Rust's ownership model, concurrency primitives, and error handling mechanisms. Here's a breakdown of the exercise:

**Exercise: Concurrent Web Server in Rust**

**Objective:** Build a concurrent web server in Rust that can handle multiple incoming requests simultaneously.

**Requirements:**
1. The web server should be able to handle HTTP GET and POST requests.
2. Implement a basic routing mechanism to serve different endpoints with appropriate responses:
   * GET "/hello" returns string: "Hello, World!" and 200 OK
   * GET "/" returns 404 page not found
   * GET "/greet/Mariusz" returns JSON `{ "greet": "Hello", "name": "Mariusz"}` and `200 OK`
   * POST "/greetme" with JSON input `{ "input": "whatever text", "name": "Mariusz"}` returns JSON `{"greet": "whatever text", "name": "Mariusz"}`
3. Utilize Rust's concurrency primitives, such as threads or async/await, to handle multiple incoming requests concurrently.
4. Ensure proper error handling for various scenarios, such as invalid requests or server errors.

**Challenges:**
1. Understanding Rust's ownership model and implementing it effectively to handle shared mutable state safely between threads.
2. Using Rust's concurrency primitives correctly to maximize performance and scalability.
3. Handling errors gracefully and efficiently, leveraging Rust's Result and Option types.
4. Implementing HTTP and JSON parsing and response generation using Rust libraries or building them from scratch.

**References:**
1. "The Rust Programming Language" book, particularly chapters on concurrency and error handling.
2. Rust documentation and official crates like `axum` for building web servers. See below.
3. Online tutorials and examples for building concurrent web servers in Rust.
4. Academic papers on concurrent programming and web server design for additional insights.


## External crates to use

Use the following crates in your implementation:

0. Use **axum** crate for building the web server. Axum is a thin layer on top of 
low-level networking API for HTTP, based on **hyper**. 

1. **tokio:** This crate provides asynchronous I/O primitives, scheduler, and runtime for building highly concurrent applications in Rust. It's commonly used in conjunction with `hyper` or `axum` for implementing asynchronous web servers.

2. **serde:** This crate is essential for serializing and deserializing data structures in Rust. Students can use `serde` to parse incoming HTTP requests and generate appropriate responses.

3. **serde_json:** You will need to work with JSON data. Use this crate for serializing and deserializing JSON in Rust.




## External crates NOT to be used

In general, avoid any additional crates beyond the ones specified above. Specifically, if ChatGPT
offers you solutions based on `hyper` directly, you should enforce it to use `axum` instead.
Solutions with other random crates will not be accepted.

* **hyper:** This is low-level api crate and you will use it for high-performance web server or client,
  but, for this exercise we DO NOT use hyper.
* **log and env_logger:** These crates provide logging utilities for Rust applications. 
  Implementing logging in the web server can help you debug issues and monitor its behavior.
  However, for this lab do not complicate your implementation with logging and do not use logging.

* **anyhow:** This crate offers convenient error handling utilities in Rust, making it 
  easier to propagate and handle errors throughout the application. It simplifies error 
  handling compared to using Rust's built-in `Result` and `Option` types directly.
  However, for this lab, use only the built-in facilities of Rust. 

* **dotenv:** This crate allows loading environment variables from a `.env` file,
  which can be useful for configuring the web server without hardcoding values
  directly into the code. However, for this lab, do not use .env file.
  Use hardcoded constants in the code instead.

* **futures:** Although `tokio` provides its own implementation of futures,
  the `futures` crate offers additional utilities and combinators for working
  with asynchronous programming in Rust. This could offer some benefits, but
  for this lab, stick to `tokio` only.



