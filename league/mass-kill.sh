p.nodes --xp ssh {n} killall ghc
p.nodes --xp ssh {n} killall ltg
p.nodes --xp ssh {n} killall ruby

sleep 10

p.nodes --xp ssh {n} killall -9 ghc
p.nodes --xp ssh {n} killall -9 ltg
p.nodes --xp ssh {n} killall -9 ruby
