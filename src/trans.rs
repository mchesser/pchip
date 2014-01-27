///
/// Description: Translate from pchip to assembly
///

mod asm;

/*
system keywords:
    __clear      -- Clear
    __draw       -- Draw(Num, Num, Num)
    __random     -- Random(Num, Num)
    __set_delay  -- SetDelay(Num)
    __get_delay  -- GetDelay(Num)
    __set_sound  -- SetSound(Num)
    __get_font   -- GetFont(Num)
}


Test Syntax:

let a = 10  -> [SetV(0, 10), SetAddress(i  ), Write(1)]
let b = 50  -> [SetV(0, 50), SetAddress(i+1), Write(1)]
let c = 0   -> [SetV(0, 0),  SetAddress(i+2), Write(1)]

if a == b  -> [SetAddress(i), Read(1), Set(1, 0), SetAddress(i+1), Read(1), Compare(1, 0)]
    c = 3  -> [SetV(0, 3), SetAddress(i+2), Write(1)]

*/
