#!/usr/bin/env ruby

if ARGV.length <= 0 || ARGV[0] != 'match'
  STDERR.puts <<USAGE
./succinct.rb match 'ai1' 'ai2'
USAGE
end

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

tmpfn = "/tmp/out/#{random(2**30)}"
sh("../bin/ltg match #{ARGV[1]} #{ARGV[2]} &> #{tmpfn}")




