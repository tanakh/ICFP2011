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

def randSeed()
  return rand(2**30)
end

def diff(fns)
  hands0 = nil
  hands1 = nil

  open(fns[0],'r'){|fp| hands0 = succinct_fp(fp) }
  open(fns[1],'r'){|fp| hands1 = succinct_fp(fp) }
    
  p = 0
  t = 1
  hands0.length.times{|i|
    if hands0[i] != hands1[i]
      unless hands0[i] && hands1[i]
        return <<MSG
hand number mismatch in #{p} turn #{t} 
---
#{hands0[i].inspect}
---
#{hands1[i].inspect}
---

MSG
      end
        
      return <<MSG
wrong hand in player #{p} turn #{t} 
---
#{hands0[i].join(' ')}
---
#{hands1[i].join(' ')}
---
MSG
    end
    p = 1-p
    t+=1 if p==0
  }

  return false
end
  



if ARGV.length < 2
  STDERR.puts <<USAGE
./compare.rb siulator1 simulator2 ['./ai1 with args' ['./ai2 with args']]

if ./ai1 or ./ai2 is not specified, ./Random is used.
USAGE
exit
end

`mkdir -p #{LogDir}`
`mkdir -p #{SampleDir}`

sims = ARGV[0..1]

def randomAI()
  if rand() < 0.3
    return "../nushio/Random" 
  elsif rand() < 0.5
    return "../nushio/SayakaTestWithRandom"
  else
    return "../nushio/UnstableSayaka" 
  end
end

def make_ai
  ret = []
  ret << (ARGV[2] || randomAI() )
  ret << (ARGV[3] || randomAI() )
  return ret
end


loop {
  ais = make_ai
  
  uniquetag = "#{rand(2**30)}"
  cmds = []
  fns = []
  fnRec = []
  fnRec[0] = "#{LogDir}#{uniquetag}-p0.rec"
  fnRec[1] = "#{LogDir}#{uniquetag}-p1.rec"

  (0..1).each{|simid|
    fn = "#{LogDir}#{uniquetag}-m#{simid}.match"
    
    wrappedAis = (0..1).map{|i|
      if simid == 0 
        "./record.rb #{fnRec[i]} #{ais[i]}"
      else
        "./play.rb #{fnRec[i]}"
      end
    }

    cmd = "#{sims[simid]} match '#{wrappedAis[0]}'  '#{wrappedAis[1]}' &> #{fn}"
    sh cmd
    cmds << cmd
    fns << fn
  }

  if reason = diff(fns)
    msg = "BAD!! #{cmds[0]} != #{cmds[1]}\n#{reason}"
    STDERR.puts msg
    open(BadFn, 'a') {|fp|
      fp.puts Time::now
      fp.puts msg
    }
    sh "cp #{fns[0]} #{fns[1]}  #{fnRec[0]} #{fnRec[1]} #{SampleDir}"
  else
    msg = "good. #{cmds[0]} == #{cmds[1]}"
    STDERR.puts msg
    open(GoodFn, 'a') {|fp|
      fp.puts Time::now
      fp.puts msg
    }
  end

  fns.each{|fn|
    sh "rm #{fn}"
  }
}
