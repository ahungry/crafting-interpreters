build:
	javac -d ./build ./src/com/craftinginterpreters/lox/*.java

run:
	java -cp ./build com.craftinginterpreters.lox.Lox

jar:
	jar cvfe lox.jar Lox ./build/*

test:
	make -B build
	make -B run

.PHONY: test
