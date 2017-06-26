all:
	happy -gca ParTMLang.y
	alex -g LexTMLang.x
	ghc --make TestTMLang.hs -o TestTMLang

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocTMLang.* LexTMLang.* ParTMLang.* LayoutTMLang.* SkelTMLang.* PrintTMLang.* TestTMLang.* AbsTMLang.* TestTMLang ErrM.* SharedString.* ComposOp.* TMLang.dtd XMLTMLang.* Makefile*
	

