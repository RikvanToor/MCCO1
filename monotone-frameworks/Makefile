default: all

all:
	cd src && alex Lexer.x
	cd src && happy Parser.y
	cd src && cpphs AttributeGrammar_cpp.ag | egrep -v "^#" > AttributeGrammar.ag
	cd src && uuagc -Hdcfws AttributeGrammar.ag --module AttributeGrammar
	cd src && ghci Main.hs

clean:
	rm -f src/AttributeGrammar.hs \
		    src/Parser.hs           \
				src/Lexer.hs            \
			  src/*.hi                \
				src/*.o                 \
				src/Main                \
				src/AttributeGrammar.ag

.PHONY: default all clean
