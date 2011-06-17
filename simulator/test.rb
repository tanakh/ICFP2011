#!/usr/bin/env ruby

def skip()
  line = STDIN.gets
  line = STDIN.gets
  line = STDIN.gets
end

kihu = <<KIHU
2
0
zero
1
dec
0
1
I
0
KIHU


if ARGV[0] == '1'
  skip
end

cnt = 0
kihu.split(/\n/).each{|line|
  puts line
  STDOUT.flush
  cnt += 1
  skip if (cnt % 3 == 0)
}
