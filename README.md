PChip
=====

A simple WIP programming language for Chip8.

## Overview

This is a first attempt at a very simple programming language and compiler. This was initially created to write programs to test my [Chip8 emulator](https://github.com/quvarxa/chip8-rs) which currently has a few bugs in it. However it turned out to be far more complicated than the emulator.

Sample programs that can be compiled can be found in `examples`.

## Some notes

### Issues

* Any syntax error results in compiler failure and so can make it difficult to debug programs
* Many unsupported features
* No optimisation resulting in very inefficient programs
* Very few of the registers are used
* No constant folding
* Expressions are parsed incorrectly (e.g. `4 + 3 - 2 + 1` is parsed as `4 + (3 - (2 + 1))`)

### Missing features

* <del>Support for comments</del>
* <del>Breaking in loops</del>
* While loops
* For loops
* Functions
* Inline functions
* Several important operators (Or, Xor, Equals, NotEquals, LessThan, GreaterThan, ...)
