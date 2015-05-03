all:
	happy -gca Parmyjs_adrian.y
	alex -g Lexmyjs_adrian.x
	ghc --make Testmyjs_adrian.hs -o Testmyjs_adrian

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docmyjs_adrian.ps

distclean: clean
	-rm -f Docmyjs_adrian.* Lexmyjs_adrian.* Parmyjs_adrian.* Layoutmyjs_adrian.* Skelmyjs_adrian.* Printmyjs_adrian.* Testmyjs_adrian.* Absmyjs_adrian.* Testmyjs_adrian ErrM.* SharedString.* ComposOp.* myjs_adrian.dtd XMLmyjs_adrian.* Makefile*
	

