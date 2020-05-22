build:
	javac -d ./build ./src/com/craftinginterpreters/lox/*.java

cli-build: src/com/craftinginterpreters/tool/GenerateAst.java
	javac -d ./build-tool ./src/com/craftinginterpreters/tool/*.java

cli-run: cli-build
	java -cp ./build-tool com.craftinginterpreters.tool.GenerateAst ast-out

run:
	java -cp ./build com.craftinginterpreters.lox.Lox

lox.jar: build
	cd build && jar cvfe lox.jar com.craftinginterpreters.lox.Lox ./* && mv lox.jar ../

jar: lox.jar

test:
	make -B build
	make -B run

clean:
	-rm -fr build
	-rm -fr *.jar

.PHONY: test
