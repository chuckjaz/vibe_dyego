# AI Reasoning

## August 2, 2025

### Goal: Implement the `when` expression

I was tasked with implementing the `when` expression in the Dyego language.

### Plan

1.  **Understand the existing code:** I started by reviewing `src/parser.rs`, `src/ast.rs`, and `src/main.rs` to understand the current state of the parser and AST.
2.  **Modify the AST:** The `when` expression AST nodes were already present in `src/ast.rs`, so no changes were needed there.
3.  **Lexer changes:**
    *   Added a `When` token to the `Token` enum in `src/parser.rs`.
    *   Added a `RArrow` token for `=>`.
    *   Updated the `read_identifier` function to recognize the `when` keyword.
    *   Updated the `next_token` function to recognize `=>`.
4.  **Parser changes:**
    *   Created a `parse_when_expression` function to parse the `when` expression.
    *   Created a `parse_when_branch` function to parse the branches of the `when` expression.
    *   Updated the `parse_primary` function to call `parse_when_expression` when a `When` token is encountered.
5.  **Testing:** Added a test case for the `when` expression to `src/parser.rs` to ensure the implementation is correct.
6.  **Documentation:** Updated `README.md` to include the syntax for the `when` expression.
7.  **Commit:** Committed the changes with a descriptive message.

### Challenges

I encountered some difficulty with the `replace` tool, which failed multiple times due to incorrect `old_string` values. I switched to reading the file, modifying the content in memory, and then using `write_file` to overwrite the original file. This approach was successful.

### Outcome

The `when` expression was successfully implemented, tested, and documented. The `README.md` was updated to reflect the changes. All tests passed, and the code was committed to the repository.
