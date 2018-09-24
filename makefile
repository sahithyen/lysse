bin/lyc: bin/ main.o
	gcc -o bin/lyc main.o

bin/:
	mkdir bin/

main.o:
	gcc -c src/main.c

doc:
	doxygen doxygen.conf

clean:
	rm -fr doc/
	rm -fr bin/
	rm -f *.o
