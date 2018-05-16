#-----------------------------------------------------------------------
#  Compiling attribute grammar code
#  This Makefile only needs to be invoked after modifying files with
#  extension .ag. Otherwise it suffices to use cabal install.
#-----------------------------------------------------------------------

# make		     - build Haskell sources out of AG code
# make cleanag - remove all .hs files that are derivatives of .ag files

default: all
all: ag

AG      = uuagc -P Syntax -P StaticAnalysis/StaticChecks
AG_OPTS = -mscfrw --kennedywarren

AG_OUT_DIR=../dist/build/autogen/uuagc

# AG sources
AGSOURCES =                                       \
     $(AG_OUT_DIR)/Syntax/UHA_Pretty.hs           \
     $(AG_OUT_DIR)/Syntax/UHA_OneLine.hs          \
     $(AG_OUT_DIR)/StaticAnalysis/StaticChecks.hs \
     $(AG_OUT_DIR)/Syntax/UHA_Syntax.hs           \
     $(AG_OUT_DIR)/Parser/ResolveOperators.hs

ag : $(AGSOURCES)

cleanag :
	$(RM) $(AGSOURCES)

$(AG_OUT_DIR)/Parser/ResolveOperators.hs : \
		Parser/ResolveOperators.ag \
		Syntax/UHA_Syntax.ag
	mkdir -p `dirname $@`
	# AG ResolveOperators
	$(AG) $(AG_OPTS) --self --module=Parser.ResolveOperators Parser/ResolveOperators.ag --output=$@

$(AG_OUT_DIR)/Syntax/UHA_Pretty.hs : \
		Syntax/UHA_Pretty.ag Syntax/UHA_Syntax.ag
	mkdir -p `dirname $@`
	# AG UHA_Pretty
	$(AG) $(AG_OPTS) --module=Syntax.UHA_Pretty Syntax/UHA_Pretty.ag --output=$@

$(AG_OUT_DIR)/Syntax/UHA_OneLine.hs : \
		Syntax/UHA_OneLine.ag Syntax/UHA_Syntax.ag
	mkdir -p `dirname $@`
	# AG UHA_OneLine
	$(AG) $(AG_OPTS) --self --module=Syntax.UHA_OneLine Syntax/UHA_OneLine.ag --output=$@

$(AG_OUT_DIR)/StaticAnalysis/StaticChecks.hs : \
		Syntax/UHA_Syntax.ag StaticAnalysis/StaticChecks.ag
	mkdir -p `dirname $@`
	# AG StaticAnalysis
	$(AG) $(AG_OPTS) --self --module=StaticAnalysis.StaticChecks StaticAnalysis/StaticChecks.ag --output=$@

$(AG_OUT_DIR)/Syntax/UHA_Syntax.hs : Syntax/UHA_Syntax.ag
	mkdir -p `dirname $@`
	# AG UHA_Syntax
	$(AG) -dmr --module=Syntax.UHA_Syntax Syntax/UHA_Syntax.ag --output=$@

