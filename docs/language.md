Lysse is specifically designed for PIC16F6XX. As this µC has many constraints (Small RAM size, few instructions and so on), I decided to have a few language features. Let's look at the language.

# First snippet

Pick your first glance at Lysse:

```
import 'time'
import 'io'

// Setup
IO.selectBank(1)
IO.TRISC<0> : true
IO.selectBank(0)

// Blink
for {
  IO.PORTC<0> : true
  Time.delay(1000)
  IO.PORTC<0> : false
  Time.delay(1000)
}
```

This is a program which blinks a LED at the Pin RC0 forever. As you can see there are some special things about Lysse:

* There aren't `;` at the end of statements
* Assignments use `:` instead of `=`
* You can manipulate single bits by using `<0>`, `<1>`, etc.
* A for-loop loops forever by omitting the initialization statement, condition expression and post statement

# Influence

The syntax of Lysse is heavily influenced by the [Go](https://golang.org) syntax as I liked the simplicity of the language.

# Data types

There are just integers (`int`) and booleans (`bool`) in Lysse. This might change in the future, but that's enough right now.

* **Boolean literals**: `true`, `false`
* **Integer literals**: `42`, `0x2A`, `0b00101010`

Integers always are the size of one byte.

You can create one-dimensional arrays by using square brackets.

* **Array literal**: [1, 2, 4, 8]

# Expressions

You probably know expressions of other programming languages. It's self-explanatory.

## Arithmetic

Lysse supports the four basic arithmetic operations.

```
1 + 1 // 2
2 - 1 // 2
1 * 2 // 2
4 / 2 // 2
```

## Bitwise operations

There are two rotate operations. `<-` shifts right and puts the LSB to the MSB. `->` shifts left and puts the LSB to the MSB. The left operand is the manipulated byte and the right operand is the number of rotates.

```
0b00000001 -> 1 // 0b10000000
0b10000010 <- 2 // 0b00001010
```

If you use numbers with the operators `and`, `or`, `xor` or 'not', the logic is applied bitwise.

```
0b10101010 and 0b11110000 // 0b10100000
0b10101010 or 0b11110000 // 0b11110000
0b10101010 xor 0b11110000 // 0b01011010
not 0b10101010 // 0b01010101
```

## Comparison

Here are comparison expressions. Note that the equal operator isn't `==`.

```
2 = 4 // false
2 != 4 // true
2 < 4 // true
2 <= 4 // true
2 > 4 // false
2 >= 4 // false
```

## Boolean logic

Here are logic operations with booleans.

```
true and false // false
true or false // true
true xor true // false
not true // false
```

# Variables

Variables store a value in memory. You can declare a variable like this:

```
let a int // Default value: 0
let b int : 2
let c bool // Default value: false
let c bool : true
const meaningOfLife int : 42
```

At the beginning of the variable you define if your variable is reassignable (`const`) or not (`let`). Then comes the name of the variable. Then comes the type of the variable. Finally you can assign a value to the new variable, if you want. Note that the assignment operator is `:`.

If you assign a value on declaration, there is no need for defining the type.

```
let b : 2
let c : true
const meaningOfLife : 42
```

To assign a new value to a variable, you can do the following:

```
a : 4
c : a xor b
```

Arrays are declared and used like this.

```
const d int[4]
const e : [1, 2, a, 8, 16, 32]

const meaningOfLife : e[1] + e[3] + e[5]
```

You can't change the size of a array after declaration and the size must be fixed (not from a variable). This is due to memory restrictions.

You can access and manipulate single bits of a number by using `<index>`.

```
let sixteen : 0b00000000
sixteen<5> : true // 0b00010000
```

A single bit is treated as a boolean.

# Control flow

Control flow in Lysse is done with two different structures.

## if-statement

An if-statement decides which statement will be executed based on conditions.

```
if a = 2 {
  foo()
} elif a = 4 {
  bar()
  a : 2
} else {
  a : 4
}
```

## for-loop

In many languages a for-loop is a powerful structure. In Lysse it is even more powerful. In fact, the for-loop is the only loop needed in Lysse.

Let's begin with the for-loop you are probably familiar with.

```
for let i : 0; i < 8; i : i + 1 {
  a<i> : 1
}
```

You can drop the init-statement and the post-statement, ...

```
for ; a < 90; {
  foo()
}
```

... which is the same as this:

```
for a < 90 {
  foo()
}
```
This behaves like a while-loop and that's why Lysse has no need for other loops.

If you also drop the condition, Lysse will insert `true`.

```
for {
  blink() // I'll blink forever
}
```

# Functions

You saw many function calls in the previous chapter. Let's look at some function declarations.

```
fun foo() int {
  return 42
}

fun sum(a int, b int) int {
  return a + b
}
```

Here are example function calls.

```
let a : foo()
let b : 2

let c : sum(a, b)
```

# Scope

In a Lysse file there is the global scope and the function scope. Every variable declared inside of function is in the scope of this function and otherwise it's in the global scope.

```
let a : 2
let b : 4

fun foo() {
  let a : 38

  let c : a + b // 42
}
```

There isn't a block scope.

# Packages

Every Lysse-file represents a package. Packages are used to organize code. You can import a package using the following statement.

```
import ./some.ly
import ./io // Does import ./io/main.ly
```

To create your own Lysse-package file, you have to use the package-statement. Every global variable and functions that are beginning with capital letter gets exported.

```
package Foo

const meaningOfLife : 42
const LysseIsAwesome : true

fun GetMeaningOfLife() {
  return meaningOfLife()
}
```

You can use the Foo package as following.

```
import './foo.ly'
import './binary_display.ly'

if not Foo.LysseIsAwesome {
  let a : GetMeaningOfLife()
  BinaryDisplay.show(a)
}
```
