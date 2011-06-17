#!/usr/bin/env ruby

Logdir = '/tmp/log/'
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
  open(fns[0],'r'){|fp0|
    open(fns[1],'r'){|fp1|
      while line0 = fp0.gets
        line1 = fp1.gets
        if line0 != line1
          return true
        end
      end
    }
  }
  return false
end
  

if ARGV.length < 2
  STDERR.puts <<USAGE
./compare.rb siulator1 simulator2 ['./ai1 with args' ['./ai2 with args']]

if ./ai1 or ./ai2 is not specified, ./Random is used.
./ai1 or ./ai2 shall take random number seed as its last argument,
which the comparator will append automatically.
USAGE
end

system("ln -s ../nushio/Random Random")
`mkdir -p #{Logdir}`
`mkdir -p #{SampleDir}`

sims = ARGV[0..1]

def make_ai
  ret = []
  ret << (ARGV[2] || "./Random #{randSlot}") + " #{rand(2**30)}"
  ret << (ARGV[3] || "./Random #{randSlot}") + " #{rand(2**30)}"  
  return ret
end

ais = make_ai

uniquetag = "#{rand(2**30)}"
cmds = []
fns = []
(0..1).each{|simid|
  fn = "#{uniquetag}-#{simid}"
  cmd = "#{sims[0]} match '#{ais[0]}' '#{ais[1]}' &> #{fn}"
  sh cmd
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
