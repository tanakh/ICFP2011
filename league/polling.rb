#!/usr/bin/env ruby

require 'open-uri'

RegisterURL = 'http://www.paraiso-lang.org/Walpurgisnacht/register.txt'
ResultFn = 'result.txt'
ParticipantsFn = 'participants_yaoj.txt'
ParticipantsFn2 = 'participants.txt'

YaujDir = 'yauj/'

def sh(string)
  STDERR.puts string
  system(str)
end

class Participant
  def initialize(n,url)
    @name = n
    @url = url
  end
  attr_accessor :name, :url

  def runpath()
    YaujDir + @name + '/run'
  end
end


regs = []

open(RegisterURL, 'r') { |fp|
  counter = 0
  while line = fp.gets
    words = line.split(/\s+/)
    next if words.length < 4
    str = words[0..-4].join(' ')
    name = counter.to_s + "_" + str.gsub(/\W/,'')
    url = words[-1]
    regs << Participant::new(name, url)
    counter +=1
  end
}

open(ParticipantsFn,'w') {|fp|
  regs.each {|participant|
    fp.puts participant.runpath 
  }
  open(ParticipantsFn2, 'r') {|fp2|
    while line = fp2.gets
      fp.puts line
    end
  }
}
