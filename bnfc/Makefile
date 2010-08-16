all: clean
	mkdir tmp
	cp Hydra.cf ./tmp
	cd tmp ; bnfc -haskell Hydra.cf ; happy -gca ParHydra.y ; alex -g LexHydra.x ;
	ghc --make Preprocessor.hs -hidir ./tmp -odir ./tmp -o ./tmp/pp
	./tmp/pp ./tmp/AbsHydra.hs    ../src/Hydra/BNFC/AbsHydra.hs
	./tmp/pp ./tmp/LexHydra.hs    ../src/Hydra/BNFC/LexHydra.hs
	./tmp/pp ./tmp/LayoutHydra.hs ../src/Hydra/BNFC/LayoutHydra.hs
	./tmp/pp ./tmp/ParHydra.hs    ../src/Hydra/BNFC/ParHydra.hs
	./tmp/pp ./tmp/ErrM.hs	      ../src/Hydra/BNFC/ErrM.hs

clean:
	rm -rf ./tmp
