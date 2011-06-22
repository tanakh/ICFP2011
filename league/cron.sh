#!/bin/sh
exit 0 # Remove this to execute

source /home/yauj/.bashrc
killall ghc # kill the server
./mass-kill.sh

sleep 10
cd /home/yauj/ICFP2011/league
./polling.rb -X &> polling_log.txt
scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/

sleep 10

./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh


