# rpn-calc-basics

# Resources

* Forth
* Factor


# Notations

* Binary functions
- `10 * 2` - infix notation
- `+ 10 2` - prefix, or Polish notation
- `2 10 +` - postfix, or Reversed Polish Notation (RPN)


# Stack

* LIFO: last in first out queue
* pop: picks an element from a stack
* push: pushes an element on top of a stack

```
-- []
20      -- [20]
10      -- [10, 20]
pop     -- [20]
pop     -- []
10      -- [10]
20      -- [20, 10]
+       -- [30]

(1 + 2) * 10 = 30

10 2 1 + * 
```

## Concatenative languages 

For function composition, concatenative lanugages have some
very nice properties allowing us to write function compositions
naturally from left to right.

```rust
y = foo(x)
z = bar(y)
w = baz(z)
```
...is written in a concatenative language as a sequence of functions:[2]

```forth
x foo bar baz
```

and in Haskell that would be:

```haskell
(baz . bar . foo) x
baz . bar . foo $ x
```


