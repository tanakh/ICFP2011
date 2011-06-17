#!/usr/bin/env ruby

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
        hand << line.strip.gsub('zero','0')
      else
        hands << hand
        hand = []
        flag = false
      end
    end
    if line.index('***')
      flag = true
      STDERR.puts "*** player #{p} turn #{t}" if verbose || ((t&(t-1)==0) && p==0)
      p = 1-p
      t += 1 if p == 0
    end
  end
  return hands
end





