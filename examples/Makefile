GHC_FLAGS = -O0 -Wall

GHC_INCLUDE_FLAGS = -i../src -i../dist/build -i../dist/build/autogen

GHC_LD_FLAGS = -lsundials_nvecserial -lsundials_kinsol -lsundials_ida



GHC_MAKE_FLAGS = -fforce-recomp $(GHC_FLAGS) $(GHC_INCLUDE_FLAGS) -odir /tmp -hidir /tmp ../dist/build/cbits/nv.o $(GHC_LD_FLAGS)

GHC_PROF_FLAGS = -osuf p_o -prof -auto -auto-all  $(GHC_MAKE_FLAGS)

all: clean
#	ghc $(GHC_MAKE_FLAGS) BouncingBall.hs
#	ghc $(GHC_MAKE_FLAGS) BreakingPendulum.hs
#	ghc $(GHC_MAKE_FLAGS) Functions.hs
#	ghc $(GHC_MAKE_FLAGS) Mechanics.hs
#	ghc --make $(GHC_MAKE_FLAGS) SimpleCircuit.hs
	cd ..; cabal install; cd examples; ghc --make $(GHC_FLAGS) BreakingPendulum.hs
#	cd ..; cabal clean; cabal install; cd examples; ghc --make $(GHC_FLAGS) BreakingPendulum.hs
#	ghc $(GHC_MAKE_FLAGS) SineOsc.hs
#	ghc $(GHC_MAKE_FLAGS) VanDerPol.hs

bench: all
	date > BENCHMARK
	./BouncingBall     +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./BreakingPendulum +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./Functions        +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./Mechanics        +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./SimpleCircuit    +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./SineOsc          +RTS -s -RTS 1> /dev/null 2>> BENCHMARK
	./VanDerPol        +RTS -s -RTS 1> /dev/null 2>> BENCHMARK

prof: clean
	ghc --make $(GHC_MAKE_FLAGS) Functions.hs
	ghc --make $(GHC_PROF_FLAGS) Functions.hs
	./Functions +RTS -p -RTS > /dev/null

clean:
	rm -f *.hi *.p_o *.o *.prof *.hp *.ps *.dat BouncingBall BreakingPendulum Functions Mechanics SimpleCircuit SineOsc VanDerPol
