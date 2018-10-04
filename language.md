# Language

Lysse is specifically designed for PIC16F6XX. As this microcontroller has many constraints (Small memory size and so on), I decided to have a few language features. Let's look at the language.

## First snippet

Pick your first glance at Lysse:

```
use "./time.ly" as Time
use "./io.ly" as IO

/* Setup */
call IO.selectBank (1)
IO.TRISC[0] : 1
call IO.selectBank (0)

/* Blink forever */
for {
  IO.PORTC[0] : 1
  call Time.delay (255)
  IO.PORTC[0] : 0
  call Time.delay (255)
}
```

This is a program which blinks a LED at the Pin RC0 forever. As you can see there are some special things about Lysse:

* There aren't `;` at the end of statements
* Assignments use `:` instead of `=`
* You can manipulate single bits of a byte using C style array access syntax
* A for-loop loops forever by omitting the initialization statement, condition expression and post statement

## Data types

There are just bytes (`byte`) and bits (`bit`) in Lysse.

## Literals

* **Bit literals**: `0`, `1`
* **Byte literals**: `42`, `0x2A`, `0b00101010`, `'a'`

You can create one-dimensional arrays by using square brackets or string literals.

* **Array literal**: `[1, 2, 4, 8]`, `"Hello, world!"`

## Expressions

### Arithmetic

Lysse supports the four basic arithmetic operations.

```
1 + 1 /* 2 */
2 - 1 /* 2 */
1 * 2 /* 2 */
4 / 2 /* 2 */
```

### Bitwise operations

There are two rotate operations. `<-` shifts right and puts the LSB to the MSB. `->` shifts left and puts the LSB to the MSB. The left operand is the manipulated byte and the right operand is the number of rotates.

```
0b00000001 -> 1 /* 0b10000000 */
0b10000010 <- 2 /* 0b00001010 */
```

If you use the operators `and`, `or`, `xor` or 'not', the logic is applied for every bit.

```
0b10101010 and 0b11110000 /* 0b10100000 */
0b10101010 or 0b11110000 /* 0b11110000 */
0b10101010 xor 0b11110000 /* 0b01011010 */
not 0b10101010 /* 0b01010101 */
```

### Comparison

Comparsion expressions always output a bit. Here are comparison expressions. Note that the equal operator isn't `==`.

```
2 = 4 /* 0 */
2 != 4 /* 1 */
2 < 4 /* 1 */
2 <= 4 /* 1 */
2 > 4 /* 0 */
2 >= 4 /* 0 */
```

## Variables

Variables store a value in memory. You can declare a variable like this:

```
let a byte /* Default value: 0 */
let b byte : 2
let c bit /* Default value: 0 */
let c bit : 1
const meaningOfLife byte : 42
```

At the beginning of the variable you define if your variable is reassignable (`const`) or not (`let`). Then comes the name of the variable. Then comes the type of the variable. You also can assign a value to the new variable, if you want to. Note that the assignment operator is `:`.

To assign a new value to a variable, you can do the following:

```
a : 4
c : a xor b
```

Arrays are declared and used like this.

```
const d byte[4]
const e byte[6] : [1, 2, a, 8, 16, 32]

const meaningOfLife byte : e[1] + e[3] + e[5]
```

You can't change the size of a array after declaration and the size must be fixed (not from a variable). This is due to memory restrictions.

You can access and manipulate single bits of a byte by using `[index]`.

```
let sixteen byte : 0b00000000
sixteen[5] : 1 /* 0b00010000 */
```

A single bit is treated as a boolean.

## Control flow

Control flow in Lysse is done with two different structures.

### if-statement

An if-statement does execute the following statement if the condition does output a value greater than 0. If the condition does output 0, the else clause gets executed.

```
if a = 2 {
  call foo
} else if a = 4 {
  call bar
  a : 2
} else {
  a : 4
}
```

### for-loop

In many languages a for-loop is a powerful structure. In Lysse it is even more powerful. In fact, the for-loop is the only loop needed in Lysse.

Let's begin with the for-loop you are probably familiar with.

```
for let i byte : 0; i < 8; i : i + 1 {
  a[i] : 1
}
```

You can drop the init-statement and the post-statement, ...

```
for a < 90 {
  call foo
}
```

The snippet above behaves like a while-loop and that is why Lysse has no need for other loops.

If you also drop the condition, Lysse will insert `true`.

```
for {
  call blink /* I will blink forever */
}
```

## User defined types

There are two different ways to define own types: structures and unions.

### Structures

You can define structures, which can hold different named variables.

```
struct Position {
  x byte,
  y byte
}
```

You can use the previous defined structure in the following way:

```
let pos Positon : {
  x : 2,
  y : 4
}

call move (pos)

pos.x : 3
```

## Union

If you want to save space and have cases where you need just one of several
data types at a time. Here is an union examle.

```
struct SensorASetting (
  sensitivity byte,
  threshold byte
)

struct SensorBSetting (
  sensitivity byte,
  mode bit
)

union SensorSetting (
  A SensorASetting,
  B SensorBSetting
)
```

An union needs as much space as the biggest data structure in the union.

Bear in mind that accessing a member of an union that is not set before can
give you unexpected data.

## Functions

You saw many function calls in the previous chapter. Let's look at some function declarations.

```
funciton foo () -> (result byte) {
  result : 42
}

function sum (a byte, b byte) -> (byte) {
  result : a + b
}

function safeSum (a byte, b byte) -> (c byte, overflow bit) {
  if 255 - b >= a {
    overflow : 1
    return
  }

  c : a * a * b
}
```

Here are example function calls.

```
call foo -> (result byte)

let a : 4
let b : 2
let someResult byte

call sum (a, b) -> (sumResult byte)
call calculate (a, b) -> (someResult, err bit)
```

## Block statement

A block is a statement which includes many statements enclosed in `{}`. You already saw many block statemnts.

```
function foo () { /* begin of function block */
  call bar -> (a bit)

  if a { /* begin of if block */
    call doSomething
  } /* end of if block */
} /* end of function block */
```

## Modules

Every Lysse-file represents a module. Packages are used to organize code. You can import a package using the following statement.

```
use "./some.ly" as some
use "./system/io.ly" as systemIO 
```

Every global variable and function that is beginning with a capital letter is accessible from the outside.

./foo.ly
```
const meaningOfLife byte : 42
const LysseIsAwesome bit : 0

function GetMeaningOfLife -> (mol byte) {
  mol = meaningOfLife
}
```

You can use the newly created module as following.

./main.ly
```

if not Foo.LysseIsAwesome {
  call Foo.GetMeaningOfLife -> (a byte)
  call BD.Show (a)
}
```

# Scopes
There are 2 different types of scopes: module scope and block scope.

In module scope you can import modules, add variables and add functions. These are then available in the whole module (file).

In block scope there are only block variables. These are only in the block and inner blocks.
