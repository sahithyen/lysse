# Scanner

The scanner transforms a stream of characters into a list of tokens.

## Token

A token has a type, lexeme, value, start position and end position.

## Tokens

### Keywords

| Token    | RegEx      | 
|----------|------------| 
| BYTE     | `byte`     | 
| BIT      | `bit`      | 
| STRUCT   | `struct`   |
| UNION    | `union`    |
| AND      | `and`      | 
| OR       | `or`       | 
| XOR      | `xor`      | 
| NOT      | `not`      | 
| LET      | `let`      | 
| CONST    | `const`    | 
| IF       | `if`       | 
| ELSE     | `else`     | 
| FOR      | `for`      | 
| FUNC     | `func`     | 
| RETURN   | `return`   | 
| IMPORT   | `import`   | 
| PACKAGE  | `package`  | 

### Literals and identifier

| Token      | RegEx                 | 
|------------|-----------------------| 
| NUMBER     | `[0-9]+`              | 
| NUMBER     | `0x[A-Za-z0-9]+`      | 
| NUMBER     | `0b[0-1]+`            | 
| CHAR       | '.'                   |
| STRING     | `".*"`                | 
| IDENTIFIER | `[A-Za-z][A-Za-z0-9]` | 

### Operators and other

| Token      | RegEx | 
|------------|-------| 
| L_SQ_BRACK | `\[`  | 
| R_SQ_BRACK | `\]`  | 
| L_RO_BRACK | `\(`  | 
| R_RO_BRACK | `\)`  | 
| L_CU_BRACK | \\{   | 
| R_CU_BRACK | \\}   | 
| COMMA      | `,`   | 
| PLUS       | `\+`  | 
| MINUS      | `-`   | 
| DIVISION   | `/`   | 
| TIMES      | `\*`  | 
| L_ROTATE   | `<-`  | 
| R_ROTATE   | `->`  | 
| EQUAL      | `=`   | 
| INEQUAL    | `!=`  | 
| SMALLER    | `<`   | 
| SMALLER_EQ | `<=`  | 
| GREATER    | `>`   | 
| GREATER_EQ | `>=`  | 
| COLON      | `:`   | 
| SEMICOLON  | `;`   | 
| DOT        | `.`   | 
