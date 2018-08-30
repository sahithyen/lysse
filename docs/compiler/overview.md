The compiler of Lysse is written in C. It has five stages.

# Pipeline

Here is an overview of every stage and the input/output of every stage.

```
           Lysse
             |
             |
            \|
  +---------------------+
  |                     |
  |      Scanning       |
  |                     |
  +---------------------+
             |
             | Tokens
            \|
  +---------------------+
  |                     |
  |       Parsing       |
  |                     |
  +---------------------+
             |
             | Syntax tree
            \|
  +---------------------+
  |                     |
  |      Analysis       |
  |                     |
  +---------------------+
             |
             | Attributed syntax tree
            \|
  +---------------------+
  |                     |
  |     Optimizing      |
  |                     |
  +---------------------+
             |
             | Attributed syntax tree
            \|
  +---------------------+
  |                     |
  |   Code generation   |
  |                     |
  +---------------------+
             |
             |
            \|
        Machine code
```

Every stage will be described in a seperate chapter.

# Target

Lysse will be compiled to run only on PIC16F6XX.

> now it is
