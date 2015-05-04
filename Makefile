all:
	cabal update
	cabal install mtl
	happy -gca Parmyjs.y
	alex -g Lexmyjs.x
	ghc --make interpreter.hs -o interpreter

clean:
	-rm -f interpreter *.log *.aux *.hi *.o *.dvi
	-rm -f Docmyjs.ps

distclean: clean
	-rm -f Docmyjs.* Lexmyjs.* Parmyjs.* Layoutmyjs.* Skelmyjs.* Printmyjs.* Testmyjs.* Absmyjs.* Testmyjs ErrM.* SharedString.* ComposOp.* myjs.dtd XMLmyjs.* Makefile*
	

