/home/management/p.nodes  --xp ssh {n} killall ghc
/home/management/p.nodes  --xp ssh {n} killall ltg
/home/management/p.nodes --xp ssh {n} killall ruby
/home/management/p.nodes --xp ssh {n} killall run

sleep 10

/home/management/p.nodes --xp ssh {n} killall -9 ghc
/home/management/p.nodes --xp ssh {n} killall -9 ltg
/home/management/p.nodes --xp ssh {n} killall -9 ruby
/home/management/p.nodes --xp ssh {n} killall -9 run
