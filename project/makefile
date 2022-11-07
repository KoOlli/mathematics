fl = -Wall -Werror -Wextra -std=c11
fl =  
ifeq ($(shell uname), Linux)
	libraries = -lcheck -lsubunit -lrt -lm -lpthread
else ifeq ($(shell uname), Darwin)
	libraries = -lcheck
endif

all: clean test
my_math.a: my_math.o
	ar -crs my_math.a my_math.o
my_math.o:
	gcc $(fl) -c my_math.c
test: my_math.a
	checkmk clean_mode=1 file.check > test.c
	gcc $(fl) my_math.c test.c $(libraries)
	./a.out
valgrind-test: test
	CK_FORK=no valgrind -s --tool=memcheck --leak-check=full --show-leak-kinds=all --show-reachable=yes ./a.out
gcov_report: my_math.a
	checkmk clean_mode=1 file.check > test.c
	gcc $(fl) --coverage test.c -o test my_math.c $(libraries)
	./test
	lcov -t "test" -o test.info -c -d .
	genhtml -o report test.info
	open report/index.html
rebuild:
	rm -f *.o
	rm -f *.a
	make
clean:
	rm -f *.o
	rm -f *.a
	rm -f *.gcda
	rm -f *.gcno
	rm -f *.info
	rm -f test
	rm -f *.out
	rm -rf report
	rm -f test.c
