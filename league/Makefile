all : nostderr LeagueServer
nostderr : nostderr.hs
	ghc --make -O3 -threaded nostderr.hs -o nostderr

LeagueServer : LeagueServer.hs
	ghc --make -O3 -threaded LeagueServer.hs -o LeagueServer
