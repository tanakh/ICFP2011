#!/bin/sh
killall  ghc
killall  ltg
killall  ruby
sleep 10
killall -9 ghc
killall -9 ltg
killall -9 ruby
sleep 10
cd /home/yauj/ICFP2011/league
./polling.rb -X &> polling_log.txt
scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/
./LeagueServer.hs &
sleep 10
./LeagueClient.hs &
sleep 10
./LeagueClient.hs &
sleep 10
./LeagueClient.hs &
sleep 10
./LeagueClient.hs &
sleep 10

