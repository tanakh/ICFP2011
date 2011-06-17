#!/usr/bin/env ruby

require 'open3'
$: << '../comparator/'
require 'succinct'

if ARGV.length <= 0 || ARGV[0] != 'match'
  STDERR.puts <<USAGE
./succinct.rb match 'ai1' 'ai2'
USAGE
end

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

sh "mkdir /tmp/out/"
tmpfn = "/tmp/out/#{rand(2**30)}"
Open3.popen3("../bin/ltg match '#{ARGV[1]}' '#{ARGV[2]}'") {|stdin, stdout, stderr|
  stdin.close
  stdout.close
  succinct_fp(stderr, true)
}







