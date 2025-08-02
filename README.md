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
    -   `val <name> = <expression>`: Declares an immutable variable.
    -   `var <name> = <expression>`: Declares a mutable variable.
-   **Function Declaration**:
    -   `fun <name>(<params>)[: <return_type>] { ... }`: Declares a function.
-   **Value Type Declaration**:
    -   `value <name>(<fields>) { ... }`: Declares a new value type.

### Expressions

Dyego is an expression-based language. The following constructs are expressions:

-   **Literals**: `123`, `123.45`
-   **Identifiers**: `my_variable`
-   **Binary Operations**: `a + b`, `a * b`, `a == b`, `a && b`
-   **Unary Operations**: `-a`, `!a`
-   **Grouped Expressions**: `(a + b)`
-   **If-Else**: `if <condition> { <then_branch> } else { <else_branch> }`
-   **Blocks**: `{ <statement>* <expression>? }`

### Types

-   **Simple Types**: `i32`, `f64`, or any user-defined type.

### Parameters and Fields

-   **Function Parameters**: `<name>: <type>`
-   **Value Fields**: `(val | var) <name>: <type>`