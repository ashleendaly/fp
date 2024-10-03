# Introduction to Functional Programming

Functional programming languages treat functions as **first-class primitives**.

This means they can be stored in variables, passed as arguments into other functions, returned from other functions and stored in data structures. Functions are just as primitive as numbers and strings in functional programming languages.

---

Imperative languages describe a sequence of steps to compute a result, focusing on how a computation is performed. However, functional languages describe what the result should be, by focusing on reducing an expression to a value.

Example:

```
addOne 3
addOne x = x + 1
```

Reducing the expression `addOne 3` to a value would mean replacing the function `addOne` with its definition and then performing a calculation:

```
3 + 1
4
```

Hence, the expression `addOne 3` was reduced to `4`.

On the contrary, imperative programming would involve defining a sequence of steps that explicitly manipulate variables in memeory.

---

> Haskell is a **general-purpose**, **statically-typed**, **purely functional** programming language with **type inference** and **lazy evaluation**.


