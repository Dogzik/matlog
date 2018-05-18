.PHONY: pack all run clean
OCAMLC=ocamlc # ocamlopt for speed
PACKAGE=hw4.zip

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run: main.exe
	./main.exe

main.exe: src/main.ml
	cd src && $(OCAMLC) str.cma main.ml -o ../main.exe

pack: src/main.ml
	zip $(PACKAGE) -r Makefile src

$(GENERATED): src/lexer.mll src/parser.mly
	
clean:
	cd src && $(DEL) *.c* *.o*
	$(DEL) main.exe $(PACKAGE)

