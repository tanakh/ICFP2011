#!/usr/bin/env ruby

def succinct(str)
  hands = []
  flag = false
  hand = []
  str.split(/\n/).each {|line|
    if flag
      if line.index('=')
        hand << line.strip.gsub('zero','0')
      else
        hands << hand
        hand = []
        flag = false
      end
    end
    flag = true if line.index('***')
  }
  return hands
end

def succinct_fp(fp, verbose = false)
  hands = []
  flag = false
  hand = []

  p = 0
  t = 1

  while line = fp.gets
    if flag
      if line.index('=')
        STDERR.puts line if verbose
        hand << line.strip
      else
        hands << hand
        hand = []
        flag = false
      end
    end
    if line.index('***')
      flag = true
      STDERR.puts "*** player #{p} turn #{t}"
      p = 1-p
      t += 1 if p == 0
    end
  end
  return hands
end





