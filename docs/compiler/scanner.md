The scanner returns the corresponding tokens from the Lysse code.

# Token

A token has a type, text, start position and end position.

```
+-----------------+
| Token           |
+-----------------+
| Type: string    |
| Text: string    |
| Start: Position |
| End: Position   |
+-----------------+
```

# Tokens

## Keywords

| Token    | RegEx      | 
|----------|------------| 
| INT      | `int`      | 
| BOOL     | `bool`     | 
| TRUE     | `true`     | 
| FALSE    | `false`    | 
| AND      | `and`      | 
| OR       | `or`       | 
| XOR      | `xor`      | 
| NOT      | `not`      | 
| LET      | `let`      | 
| CONST    | `const`    | 
| IF       | `if`       | 
| ELIF     | `elif`     | 
| ELSE     | `else`     | 
| FOR      | `for`      | 
| CONTINUE | `continue` | 
| BREAK    | `break`    | 
| FUN      | `fun`      | 
| RETURN   | `return`   | 
| IMPORT   | `import`   | 
| PACKAGE  | `package`  | 

## Literals and name

| Token      | RegEx                 | 
|------------|-----------------------| 
| DEC_NUMBER | `[0-9]+`              | 
| HEX_NUMBER | `0x[A-Za-z0-9]+`      | 
| BIN_NUMBER | `0x[0-1]+`            | 
| STRING     | `'.*'`                | 
| NAME       | `[A-Za-z][A-Za-z0-9]` | 

## Operators and other

| Token      | RegEx | 
|------------|-------| 
| L_SQ_BRACK | `\[`  | 
| R_SQ_BRACK | `\]`  | 
| L_RO_BRACK | `\(`  | 
| R_RO_BRACK | `\)`  | 
| L_CU_BRACK | `\{`  | 
| R_CU_BRACK | `\}`  | 
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
