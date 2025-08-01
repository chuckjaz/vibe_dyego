# Dyego

Dyego is a new programming language centered around mutable value semantics.

## Installation

(Instructions to be added)

## Usage

(Instructions to be added)

### Value Type Declaration

Value types can be declared using the `value` keyword. They can contain both `val` (immutable) and `var` (mutable) fields.

```dyego
value MyValueType(val immutableField: i32, var mutableField: String) {
    // Methods and other logic can go here
}
```

### Expression Syntax

Dyego supports various expressions, including arithmetic operations, comparisons, logical operations, and more.

```dyego
val sum = 10 + 5 * 2
var diff = 20 - (7 / 3)
val is_positive = -5 < 0
val result = (true && false) || true
```
