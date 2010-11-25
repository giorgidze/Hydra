GHC_FLAGS       =  -O3 -Wall -fforce-recomp
GHC_FLAGS_PROF  =  $(GHC_FLAGS) -osuf p_o -prof -auto -auto-all  

MODEL = SineOsc

all: clean
		ghc --make $(GHC_FLAGS) $(MODEL).hs

prof: all
		ghc --make $(GHC_FLAGS_PROF)  $(MODEL).hs
		./$(MODEL) +RTS -p -RTS > /dev/null

run: all
		./$(MODEL)

runall: clean
		runghc SineOsc.hs
		runghc VanDerPol.hs
		runghc Functions.hs
		runghc BouncingBall.hs
		runghc BreakingPendulum.hs
		runghc Electronics.hs
		runghc Mechanics.hs

plot: all
		./$(MODEL) > plot.dat
		gnuplot -persist plot

clean:
		rm -f *.hi *.p_o *.o *.prof *.hp *.ps *.dat $(MODEL)
