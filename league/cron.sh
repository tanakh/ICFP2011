#!/bin/sh
exit 0 # Remove this to execute

cd $HOME/ICFP2011/league/

source /home/yauj/.bashrc
killall ghc # kill the server

scp scoreboard.html paraiso-lang.org:/var/www/html/Walpurgisnacht/store/scoreboard2.html

./mass-kill.sh

sleep 10
cd /home/yauj/ICFP2011/league
./polling.rb -X &> polling_log.txt
scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/

sleep 10
./LeagueServer.hs &
sleep 1

./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh


