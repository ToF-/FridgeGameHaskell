C=ghc
F=--make -threaded 

specs: Specs.hs RefrigeratedRoom.hs Simulation.hs Report.hs
	$(C) --make Specs.hs -o bin/specs
	bin/specs

simpleserver: SimpleServer.hs
	$(C) $(F) SimpleServer.hs -o bin/server

simserver: SimulationServer.hs
	$(C) $(F) SimulationServer.hs -o bin/simserver

timer: SimpleTimerExample.hs
	$(C) $(F) SimpleTimerExample.hs -o bin/timer

runner: SimpleSimulationRunner.hs
	$(C) $F SimpleSimulationRunner.hs -o bin/runner

console: TextSimulationRunner.hs RefrigeratedRoom.hs Simulation.hs Report.hs
	$(C) $F TextSimulationRunner.hs -o bin/fridge


clean:
	rm *.hi
	rm *.o
