#!/usr/bin/env ruby

require 'open-uri'
require 'fileutils'

RegisterURL = 'http://www.paraiso-lang.org/Walpurgisnacht/register.txt'
ResultFn = 'result.txt'
ParticipantsFn = 'participants_yaoj.txt'
ParticipantsFn2 = 'participants.txt'
Ptgz = 'p.tgz'

YaujDir = 'yauj/'

ExecMode = ARGV.index('-X')

def sh(str)
  STDERR.puts str
  system(str)
end



class Participant
  def initialize(n,url)
    @name = n
    @url = url
  end
  attr_accessor :name, :url

  def localdir()
    YaujDir + @name + '/'
  end
  def packagepath()
    localdir() + Ptgz
  end
  def runpath()
    "./" + localdir() + 'run'
  end

end


yaojParticipants = []

open(RegisterURL, 'r') { |fp|
  counter = 0
  while line = fp.gets
    words = line.split(/\s+/)
    next if words.length < 4
    str = words[0..-4].join(' ')
    name = counter.to_s + "_" + str.gsub(/\W/,'')
    url = words[-1]
    yaojParticipants << Participant::new(name, url)
    counter +=1
  end
}

open(ParticipantsFn,'w') {|fp|
  yaojParticipants.each {|pants|
    fp.puts pants.runpath 
  }
  open(ParticipantsFn2, 'r') {|fp2|
    while line = fp2.gets
      fp.puts line
    end
  }
}

yaojParticipants.each{|pants|
  sh "rm -fr #{pants.localdir}"
  sh "mkdir #{pants.localdir}"
  sh "wget #{pants.url} -q  -O #{pants.packagepath}"
  FileUtils.cd(pants.localdir) {
    sh "tar zxf #{Ptgz}"
    contents = `tar tf #{Ptgz}`.split(/\n/)
    flag = false
    contents.each{|fn|
      words = fn.split('/')
      if words[-1] == 'install'
        flag = true# found
        if words.length >= 2
          path = words[0..-2].join('/')
          sh "mv #{path}/* ."
        end
        break
      end
    } # end search for
    unless flag
      STDERR.puts "!! cannot recognize package for #{pants.name}"
      next
    end
    sh "install" if ExecMode
  } # exit participant directory
} # participant loop
