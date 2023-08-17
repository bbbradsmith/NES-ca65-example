# Minimal NES example using ca65

This is a small example program for the NES, intending to demonstrate how to use ca65 to build an NES ROM.
It is a very basic starting point for programming the NES.

There is also a version of this example available to build an FDS disk image for the Famicom Disk System.

* NES: https://github.com/bbbradsmith/NES-ca65-example
* FDS: https://github.com/bbbradsmith/NES-ca65-example/tree/fds

This depends on the CC65 toolchain, though it only uses the ca65 assembler, not the C compiler:

https://cc65.github.io/

To install cc65 on Mac OS using [Homebrew](https://brew.sh/) run `brew install cc65`.

See example.txt for more details.

This project was originally distributed at the NESDev forums:

https://forums.nesdev.com/viewtopic.php?t=11151

Unix users can use the provided bash script, or if make is available the Makefile can be used.

```
compile_example.sh   # Bash script to build project
make                 # Builds project - incremental build
make -B              # Builds project - full build
make clean           # Cleans (deletes) build output
make nl              # Runs python script to generate nl files for fceux.
make test            # Runs built game in emulator (fceux)
```

Unlike the Windows example, this script and makefile assume the cc65 tools are installed globally,
or otherwise available on the current search paths. If needed, you can edit them or provide
environment variables to select a different location.

## License
This program and source code can be freely redistributed, modified or reused in any way.
Credit is not required, but attribution is always appreciated.

Brad Smith

http://rainwarrior.ca
