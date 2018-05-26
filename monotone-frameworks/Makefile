default: all

all: src/Lexer.hs src/Parser.hs src/AttributeGrammar.hs

dev: all
	cd src && ghci Dev

#----------------------------

src/Lexer.hs:
	cd src && alex Lexer.x

src/Parser.hs:
	cd src && happy Parser.y

# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions
src/AttributeGrammar.hs:
	cpp -w src/AttributeGrammar_cpp.ag | egrep -v "^#" > src/AttributeGrammar.ag
	cd src && uuagc -Hdcfws AttributeGrammar.ag --module AttributeGrammar

#----------------------------

clean:
	rm -f src/AttributeGrammar.hs \
		    src/Parser.hs           \
				src/Lexer.hs            \
			  src/*.hi                \
				src/*.o                 \
				src/Main                \
				src/AttributeGrammar.ag

.PHONY: default all clean
