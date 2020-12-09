all:
	happy -gca Parsing/ParLatte.y
	alex -g Parsing/LexLatte.x
	ghc --make Parsing/TestLatte.hs -o Parsing/TestLatte

clean:
	-rm -f Parsing/*.log Parsing/*.aux Parsing/*.hi Parsing/*.o Parsing/*.dvi

distclean: clean
	-rm -f Parsing/DocLatte.* Parsing/LexLatte.* Parsing/ParLatte.* Parsing/LayoutLatte.* Parsing/SkelLatte.* Parsing/PrintLatte.* Parsing/TestLatte.* Parsing/AbsLatte.* Parsing/TestLatte Parsing/ErrM.* Parsing/SharedString.* Parsing/ComposOp.* Parsing/Latte.dtd Parsing/XMLLatte.* Makefile*
		-rmdir -p Parsing/
