## Expression vs Statements

> In a functional language, everything is an expression.

|Statement | Expression |
|----|----|
| An instruction that performs an action (e.g. modify a variable) | A term that evaluates to a value (e.g. string, integer, function) |
| May not return a value | Always returns a value |

### Statements

```
x = 5    # Statement: assigns the value 5 to variable x
print(x) # Statement: prints the value of x
```

## Conditional Statements as Expressions

```
if 2 < 3 then 0
else 1
```
Haskell conditionals are expressions, meaning they must always reduce to a value. This is why they must have an else clause.

Since conditionals are expressions, they can be used in any place where a value is expected, such as function defintion, assignment or insight a larger expression.

```
a = if 3 > 2 0 else 1
```

This is different to imperative programming languages where conditonals are **statements** and are used to control the flow of the program. Their main purpose is to conditionally execute blocks of code, instead of return a value.

```
if 2 < 3:
    result = 0
else:
    result = 1

```

## Types

In Haskell, each expression has a type

- `5` has the type `Int`
- `True` has the type `Bool`
- `(1, "Hello")` is a tuple with types `(Int, String)`.

Haskell is **statically typed**, meaning that types are checks at compile time. Before the program is run, the compiler ensures that all expressions have valid types. If there is a type mismatch, such as trying to perform an operation on incompatible types (e.g., `5 + True`), the program will not compile.

Haskell can also infer types, meaning that the compiler can infer the types of most expressions without the programmer needing to explicitly declare them. However, adding type annotations anyway makes the code clearer and is best practice.