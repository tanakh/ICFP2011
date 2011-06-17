#!/usr/bin/env ruby

$: << '.'
require 'succinct'


LogDir = '/tmp/log/'
SampleDir = 'sample/'
GoodFn = 'goodnews.log'
BadFn  = 'badnews.log'

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

def randSlot()
  return [1,2,4,8,16,32,rand(256)].map{|x| [rand, x]}.sort[0][1]
end

def diff(fns)
  str0 = open(fns[0],'r').read
  str1 = open(fns[1],'r').read
  
  hands0 = succinct(str0)
  hands1 = succinct(str1)
  
  return hands0 != hands1
end
  

if ARGV.length < 2
  STDERR.puts <<USAGE
./compare.rb siulator1 simulator2 ['./ai1 with args' ['./ai2 with args']]

if ./ai1 or ./ai2 is not specified, ./Random is used.
./ai1 or ./ai2 shall take random number seed as its last argument,
which the comparator will append automatically.
USAGE
exit
end

system("ln -s ../nushio/Random Random")
`mkdir -p #{LogDir}`
`mkdir -p #{SampleDir}`

sims = ARGV[0..1]

def make_ai
  ret = []
  ret << (ARGV[2] || "./Random #{randSlot}") + " #{rand(2**30)}"
  ret << (ARGV[3] || "./Random #{randSlot}") + " #{rand(2**30)}"  
  return ret
end


loop {
  ais = make_ai
  
  uniquetag = "#{rand(2**30)}"
  cmds = []
  fns = []
  (0..1).each{|simid|
    fn = "#{LogDir}#{uniquetag}-#{simid}"
    cmd = "#{sims[simid]} match '#{ais[0]}' '#{ais[1]}' &> #{fn}"
    sh cmd
    cmds << cmd
    fns << fn
  }

  if diff(fns)
    msg = "BAD!! #{cmds[0]} != #{cmds[1]}"
    STDERR.puts msg
    open(BadFn, 'a') {|fp|
      fp.puts msg
    }
    sh "cp #{fns[0]} #{fns[1]} #{SampleDir}"
  else
    msg = "good. #{cmds[0]} == #{cmds[1]}"
    STDERR.puts msg
    open(GoodFn, 'a') {|fp|
      fp.puts msg
    }
  end

  fns.each{|fn|
    sh "rm #{fn}"
  }
}
