bin/lyc: main.o
	gcc -o bin/lyc main.o

main.o:
	gcc -c src/main.c

clean:
	rm bin/lyc
	rm *.o
