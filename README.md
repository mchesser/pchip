Pchip
=====

A simple programming language for Chip8.

## Overview
Pchip is designed as a toy programming language and compiler. The syntax is inspired by the [Rust programming language](http://www.rust-lang.org/), however it is much simpler. 

### Syntax

(Note: The syntax here is a work in progress)

Variables are declared using the `let` keyword. All variables are `u8`.

    let a = 10;


There are 3 types of loops supported by Pchip. These are declared using the following keywords: `loop` `while` (currently unsupported) and `for` (currently unsupported).

    loop {
      break;
    }


Pchip supports (currently some issues exist) control flow in the form of `if` statements.

    if <condition> {
        ...
    }
    else {
        ...
    }

Note that the else block is optional.


### Built-in functions

* `clear()` -- Clears the screen
* `draw_pos(x, y)` -- Sets the drawing position to the coordinates (`x`, `y`)
* `draw5(address)` -- Draws 5 lines of data from `address` at the draw position
* `get_font(char_code)` -- Gets the address of `char_code` in the glyphs table
* `key_wait()` -- Waits for a key to be pressed and returns the pressed key
* `plus(a, b)` -- Returns a + b

### Examples

Example programs that can be compiled using Pchip can be found in `examples`.


## Some notes

### History

Pchip was initially created to write programs to test my [Chip8 emulator](https://github.com/quvarxa/chip8-rs) which currently has a few bugs in it. However it turned out to be far more complicated than the emulator.

### Issues

* Any syntax error results in compiler failure and so can make it difficult to debug programs
* Many unsupported features
* No optimisation resulting in very inefficient programs
* Very few of the registers are used
* No constant folding

### Missing features

* <del>Support for comments</del>
* <del>Breaking in loops</del>
* Support for infix operators
* While loops
* For loops
* Functions
* Inline functions
* Several important operators (Or, Xor, Equals, NotEquals, LessThan, GreaterThan, ...)
