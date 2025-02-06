# Brad (insert acronym) (the language)

## Types

### Primitives

 - `int`
 - `float`
 - `str`

### Generics

### Composites

 - references `ref 'a`
 - tasks `task 'a`
 - lists `['a]`
 - array `['a; N]`
 - functions `'a -> 'b -> 'c`
 - tuple `'a & 'b`
 - unions `'a | 'b`
 - records `{ a: 'a, b: 'b }`

### Named types

Naming a type is a way to bind a name to a type, and give that type a seperate
identity from the type named. See the example below:

```
type named = float

// you cannot have multiple instances of the same type as different variants of a union
type fails = float | float

// you can get around this by using named types
type works = float | named
```

A named type is can be implicitly cast to it's inner type, but the reverse is not possible.

### Builtins

 - `bool = true | false`
 - `error<'e> = 'e`
 - `true`
 - `false`
 - `none`

## Functions

Functions are defined with a series of typed arguments and a return type, if
the return type is not specified it defaults to `none`.

```
fn fib(n: int) -> int {
    if n < 2 -> return 1
    (fib n - 1) + (fib n - 2)
}
```

## Expressions

### Literals

 - Strings `'hello, world!'` or `"hello, world!"`.
 - Numbers `1`, `1.0` or `1e+0`.
 - Lists `[1, 'hello', c]`.

### Let and variables

A variable is declared with `let` a pattern, an optional type and an optional value.

```
let var: int | none = int
```

### Calls

Calling a function is done by writing the callee expression followed by a
number of argument expressions, e.g. `foo 1.0 false`.

### Do and await

The `do` expression runs an expression asynchonously, that can later be
`await`ed. `await` takes a task, waits for it to complete and returns the
result.

```
let t: task int = do fib(100) 
let r: int = await t
```

### Operands

## Patterns

## Keywords
