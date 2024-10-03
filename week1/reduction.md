# Reduction

Reducations takes an expresssion and eventually produces a value.

Functional languages use reduces expressions during computation, whereas imperative programming languages focus on storing state in memory and altering the state using statements.

## Reduction Example

```
1 + 2 + 3 + 4
1 + 2 + 7
1 + 9
10
```

Here an expression is reduced to a value.

> Church-Rosser property: Reductions can be performed in **any order**

For example, the above example could be reduced such as

```
1 + 2 + 3 + 4
3 + 3 + 4
6 + 4
10
```