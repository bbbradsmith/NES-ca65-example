# Minimal NES example using ca65

This is a small example program for the NES, intending to demonstrate how to use ca65 to build an NES ROM.
It is a very basic starting point for programming the NES.

There is also a version of this example available to build an FDS disk image for the Famicom Disk System.

* NES: https://github.com/bbbradsmith/NES-ca65-example
* FDS: https://github.com/bbbradsmith/NES-ca65-example/tree/fds

This depends on the CC65 toolchain, though it only uses the ca65 assembler, not the C compiler. Download the "snapshot" here:

https://cc65.github.io/

See example.txt for more details.

This project was originally distributed at the NESDev forums:

https://forums.nesdev.com/viewtopic.php?t=11151

For instructions building this example on Unix-like systems or MacOS, see:
* Unix: https://github.com/bbbradsmith/NES-ca65-example/tree/unix

Unix users that have gnu make installed can build just by running the following commands:

```make       ```    # Builds project - incremental build

```make -B    ```    # Builds project - full build

```make clean ```    # Cleans (deletes) build output

```make nl    ```    # Runs python script to generate nl files for fceux.

The Makefile assumes that the ca65 assembler and the ld65 linker executables are in the shell's execution $PATH.
If these programs are not acessible via $PATH, then edit the Makefile to give the proper path to these files.

## License
This program and source code can be freely redistributed, modified or reused in any way.
Credit is not required, but attribution is always appreciated.

Brad Smith

http://rainwarrior.ca
