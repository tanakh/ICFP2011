#!/usr/bin/env ruby

def succinct(str)
  hands = []
  flag = false
  hand = []
  str.split(/\n/).each {|line|
    if flag
      if line.index('=')
        hand << line.strip
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




