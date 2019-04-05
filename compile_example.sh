#!/usr/bin/env sh
set -e
ca65 example.s -g -o example.o
ld65 -o example.nes -C example.cfg example.o -m example.map.txt -Ln example.labels.txt --dbgfile example.nes.dbg
python example_fceux_symbols.py
