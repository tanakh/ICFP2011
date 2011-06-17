#!/usr/bin/env ruby

$: << '../comparator/'
require 'succinct'

if ARGV.length <= 0 || ARGV[0] != 'match'
  STDERR.puts <<USAGE
./succinct.rb match 'ai1' 'ai2'
USAGE
end

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end


sh "mkdir /tmp/out/"
tmpfn = "/tmp/out/#{rand(2**30)}"
sh("../bin/ltg match '#{ARGV[1]}' '#{ARGV[2]}' &> #{tmpfn}")

p = 0
t = 1
succinct(open(tmpfn,'r').read).each{|hand|
  STDERR.puts "*** player #{p} turn #{t}"
  hand.each{|line|
    STDERR.puts line
  }
  STDERR.puts "omitted"
  p = 1-p
  t += 1 if p == 0
}


sh "rm #{tmpfn}"


