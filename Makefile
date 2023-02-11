
GAME=example
ASSEMBLER=ca65
LINKER=ld65
PYTHON=python3
EMULATOR=fceux

OBJ_FILES=example.o

all: $(GAME).nes

$(GAME).nes : $(OBJ_FILES)  $(GAME).cfg
	$(LINKER) -o $(GAME).nes -C $(GAME).cfg $(OBJ_FILES) -m $(GAME).map.txt -Ln $(GAME).labels.txt --dbgfile $(GAME).nes.dbg

clean:
	rm -f *.o *.nes *.dbg *.nl *.map.txt *.labels.txt

test: $(GAME).nes
	$(EMULATOR) $(GAME).nes

nl: $(GAME).nes
	$(PYTHON) $(GAME)_fceux_symbols.py

example.o: example.s sprite.chr background.chr

%.o:%.s
	$(ASSEMBLER) $< -g -o $@

