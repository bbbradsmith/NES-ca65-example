@del example.o
@del example.fds
@del example.map.txt
@del example.labels.txt
@del example.fds.ram.nl
@del example.fds.0.nl
@del example.fds.1.nl
@del example.fds.dbg
@echo.
@echo Compiling...
cc65\bin\ca65 example.s -g -o example.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
cc65\bin\ld65 -o example.fds -C example.cfg example.o -m example.map.txt -Ln example.labels.txt --dbgfile example.fds.dbg
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Generating FCEUX debug symbols...
python example_fceux_symbols.py
@echo.
@echo Success!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build error!
@pause
:endbuild
