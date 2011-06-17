#!/usr/bin/env ruby


if ARGV.length < 2
  STDERR.puts <<USAGE
./record.rb log-filename ./program

excecute the ./program, and records its standard output to log-filename.
USAGE
  exit
end

require 'open3'

LogFn = ARGV[0]
Prog  = ARGV[1..-1].join(' ')

Open3.popen3(Prog) {|stdin, stdout, stderr|
  ti = Thread.new do
    while line = STDIN.gets
      stdin.puts line
      stdin.flush
    end
    stdin.close
  end

  to = Thread.new do
    while line = stdout.gets
      STDOUT.puts line
      STDOUT.flush
    end
  end

  te = Thread.new do
    while line = stderr.gets
      STDERR.puts line
      STDERR.flush
    end
  end

  ti.join
  to.join
  te.join
}
