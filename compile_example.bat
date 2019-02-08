@del example.o
@del example.nes
@del example.map.txt
@del example.labels.txt
@del example.nes.ram.nl
@del example.nes.0.nl
@del example.nes.1.nl
@echo.
@echo Compiling...
cc65\bin\ca65 example.s -g -o example.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
cc65\bin\ld65 -o example.nes -C example.cfg example.o -m example.map.txt -Ln example.labels.txt --dbgfile example.nes.dbg
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
