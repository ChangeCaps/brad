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

Ordered in terms of precedence.

### Block

A block returns the value of its last expression.

```
{ expr* }
```

### Group

( expr )

### Literals

 - Strings `'hello, world!'` or `"hello, world!"`
 - Numbers `1`, `1.0` or `1e+0`, `0x0`, `0b1`, `0o1`

### Initializers

 - Lists `[1, 'hello', c]`
 - Record `{ field: expr, ... }`

### Operands

#### Unary

 - `!` logical not
 - `-` negation
 - `~` bitwise not
 - `&` reference 
 - `*` dereference

#### Binary

 - `*` multiplication, `/` division, `%` modulo
 - `+` addition, `-` subtraction
 - `>>`, `<<` shifts
 - `&` bitwise and
 - `^` bitwise xor
 - `|` bitwise or
 - `<=`, `>=`, `>`, `<` comparisions
 - `==`, `!=` equality
 - `&&` logical and, `||` logical or
 - `=` assignment

Follows precedence in above order.

### Do and await

The `do` expression runs an expression asynchonously, that can later be
`await`ed. `await` takes a task, waits for it to complete and returns the
result.

```
let t: task int = do fib(100) 
let r: int = await t
```

### Calls

Calling a function is done by writing the callee expression followed by a
number of argument expressions, e.g. `foo 1.0 false`.

#### Closure

If you have a closure that captures some data

#### Zero-argument functions

```
fn foo(a: int, b: int, c: int) -> int {
  a + b + c
}

let closure = fn(none) -> foo 1 2 3
let result = closure none
```

### Tuple initializer

 - Tuple `1, 2, 'Hello'`

### If

Classical if statement with branching.

```
if 2 == 3 -> print 'Second law of thermodynamics'
```

```
if 3 == 3 {
    print 'All is good in the world'
} else halt_and_catch_fire
```

Return type of this if branch is int | none

```
if x is int as y {
    // type of y is constrainted to int
    y
}
```

More generally the return type an if statement is the union of its two branches (if/else), where the default else type is `none`.

### Match

Pattern matching is for *types* only as of this time. So no matching of literals. The return type of match is the union of all its branches types.

```
type X 
| Yay = int & int
| Hmm = str


match x
| Yay as a, b -> a + b
| Hmm as y -> print y
```

### Loop

```
loop expr
```

```
loop {

}
```

```
loop match expr
| ...

```

### Control flow

#### Return

Returning without a value returns `none`.

```
return expr
```

#### Break

Breaking with a value returns that value as the result of the loop expression, no value is `none`.

```
break expr
```

#### Continue

Works as expected, goes to the next loop iteration.

```
continue
```

### Let and variables

A variable is declared with `let` a pattern, an optional type and an optional value.

```
let var: int | none = 0
```

## Patterns

 - `identifier` a name for a new variable (binding)
 - `mut identifier` a mutable binding
 - `ref identifier` a reference to the original variable
 - `pattern, pattern, ...` tuple pattern


<!-- - `{ field: pattern, ... }` record pattern -->

## Keywords

 - `as`
 - `is` remember these