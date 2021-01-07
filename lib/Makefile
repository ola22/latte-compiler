all: runtime_helper.o runtime.o

runtime_helper.o: runtime_helper.asm
	nasm -g -f elf64 runtime_helper.asm

runtime.o: runtime.c
	gcc -Wall -Wextra runtime.c -g -c

clean:
	rm -f *.o