#!/usr/bin/env ruby


if ARGV.length < 2
  STDERR.puts <<USAGE
./compare.rb siulator1 simulator2 ['./ai1 with args' ['./ai2 with args']]

if ./ai1 or ./ai2 is not specified, ./Random is used.
./ai1 or ./ai2 shall take random number seed as its last argument,
which the comparator will append automatically.
USAGE
end


