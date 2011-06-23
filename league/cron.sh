#!/bin/sh
exit 0 # Remove this to execute

cd $HOME/ICFP2011/league/

source /home/yauj/.bashrc


echo start cron at `date` > polling_log.txt


if ls yauj.mutex ; then 
    echo "previous contest is still running!" >> polling_log.txt
    scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/polling_log_current.txt
    exit 1
fi


# Backup Previous Scoreboard
scp scoreboard.html paraiso-lang.org:/var/www/html/Walpurgisnacht/store/scoreboard2.html

sleep 1
cd /home/yauj/ICFP2011/league
./polling.rb -X &>> polling_log.txt

if test $? -ne 0; then
    # There was an error in polling, or the submissions are all up to date.
    scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/polling_log_current.txt
    exit 1
fi


echo ready to start league at `date` >> polling_log.txt

scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/polling_log_current.txt
scp polling_log.txt paraiso-lang.org:/var/www/html/Walpurgisnacht/store/polling_log.txt

# Clear the worker nodes
./mass-kill.sh

sleep 1

./LeagueServer.hs &
sleep 1

./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh
sleep 10
./mass-submit.sh


