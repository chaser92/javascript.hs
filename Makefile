all:
	happy -gca Parmyjs.y
	alex -g Lexmyjs.x
	ghc --make Testmyjs.hs -o Testmyjs

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docmyjs.ps

distclean: clean
	-rm -f Docmyjs.* Lexmyjs.* Parmyjs.* Layoutmyjs.* Skelmyjs.* Printmyjs.* Testmyjs.* Absmyjs.* Testmyjs ErrM.* SharedString.* ComposOp.* myjs.dtd XMLmyjs.* Makefile*
	

