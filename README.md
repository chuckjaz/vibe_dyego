# Dyego

Dyego is a new programming language centered around mutable value semantics.

## Installation

(Instructions to be added)

## Usage

(Instructions to be added)

## Syntax

This section provides a summary of the Dyego language syntax.

### Statements

-   **Variable Declaration**:
    -   `val <name>[: <type>] = <expression>`: Declares an immutable variable.
    -   `var <name>[: <type>] = <expression>`: Declares a mutable variable.
-   **Function Declaration**:
    -   `fun <name>(<params>)[: <return_type>] { ... }`: Declares a function.
-   **Value Type Declaration**:
    -   `value <name>(<fields>) { ... }`: Declares a new value type.

### Expressions

Dyego is an expression-based language. The following constructs are expressions:

-   **Literals**: `123`, `123.45`, `"hello"`
-   **Identifiers**: `my_variable`
-   **Binary Operations**: `a + b`, `a * b`, `a == b`, `a && b`
-   **Unary Operations**: `-a`, `!a`
-   **Grouped Expressions**: `(a + b)`
-   **Tuple Expressions**: `(a, b, c)`
-   **If-Else**: `if <condition> { <then_branch> } else { <else_branch> }`
-   **When**: `when <expression> { <condition> => <result>, ... }`
-   **Blocks**: `{ <statement>* <expression>? }`

### Types

-   **Simple Types**: `i32`, `f64`, or any user-defined type.
-   **Tuple Types**: `(<type>, <type>, ...)`

### Parameters and Fields

-   **Function Parameters**: `<name>: <type>`
-   **Value Fields**: `(val | var) <name>: <type>`
