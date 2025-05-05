eclipsesim
==========

Simulator for the [Data General Eclipse](https://en.wikipedia.org/wiki/Data_General_Eclipse)
16-bit mini-computer.

Build
-----

    $ make

This builds the simulator as `${CWD}/bin/eclipsesim`.

Usage with DG Relocatable Binary files
--------------------------------------

    $ dga priv/hello.sr
    $ bin/eclipsesim hello.rb
    HELO

For this you need a cross-assembler capable of emitting DG Relocatable Binary files.
I use the `dga` assembler from [dpa](https://github.com/mikpe/dpa).

Dependencies
------------

The simulator is written in Erlang so you need that to build and run it.
