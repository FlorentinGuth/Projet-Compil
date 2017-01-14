FLAGS = -I _build/ -verbose 1 -use-menhir -yaccflags -v -lflags -g -cflags -g,-w,+A-4-50-52

TESTDIR = ./tests/exec/
TARGET = nathanael-quine

all: test

test_perso: main.byte
	./test.sh -p adac

test: main.byte
	./adac $(TESTDIR)$(TARGET).adb
	gcc -g $(TESTDIR)$(TARGET).s -o $(TESTDIR)a.out
	./$(TESTDIR)a.out

.PHONY: main.byte regen_msg update_msg clean

main.byte: 
	ocamlbuild $(FLAGS) $@
	mv $@ adac


clean:
	ocamlbuild -clean


regen_msg:
	mv parser.messages parser_sav.messages
	menhir --list-errors parser.mly >> parser.messages

update_msg:
	mv parser.messages parser_old.messages
	menhir --update-errors parser_old.messages parser.mly >> parser.messages
	rm -f parser_old.messages

parser_msg.ml: update_msg
	rm -f _build/parser_msg.ml
	menhir --compile-errors parser.messages parser.mly >> _build/parser_msg.ml
