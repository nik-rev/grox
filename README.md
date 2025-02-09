# Grox

> [!NOTE]
> This language is a work in progress!

A statically typed functional programming language inspired by Rust that compiles down to LLVM.

## Fibonacci

Last expression

```rs
fn add(a: i32, b: i32) -> i32 {
  // Last expression is the function's return value
  a + b
}

fn maybe_multiply(a: i32, b: i32, c: i32) -> i32 {
  if a > b {
    b * c
  } else if a == b - 4 {
    a
  } else {
    b
  }
}

// the program evaluates to whatever main() returns.
fn main() -> i32 {
  // all functions are called using the pipe
  // Functions are curried
  // a is a function that will add 4 to its input
  let add_4 = 4 | .add;

  let eight = 4 | .add_4;
  let minus_two = -6 | .add_4;

  // the language supports variable shadowing
  // To pass multiple arguments to a function, chain with the pipe
  let eight = 4 | 4 | .add;

  // you can extract pipes into a separate expression!
  let pipe_expr = 8 | 12;
  let twenty = pipe_expr | .add;
  // you can chain them as much as you wish!
  let minus_forty =
    pipe_expr | .add | eight | .add
      | twenty | minus_two | .maybe_multiply;

  // this small program outputs -40
  minus_forty
}
```
