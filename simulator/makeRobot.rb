#!/usr/bin/env ruby


puts <<HEADER
#!/usr/bin/env ruby

def skip()
  line = STDIN.gets
  line = STDIN.gets
  line = STDIN.gets
end

kihu = <<KIHU
HEADER

while line = STDIN.gets
  puts line
end


puts <<FOTTER
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
FOTTER


