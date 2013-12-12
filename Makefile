C=ghc
F=--make -threaded 


server: SimpleServer.hs
	$(C) $(F) SimpleServer.hs -o bin/server
