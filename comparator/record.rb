#!/usr/bin/env ruby


if ARGV.length < 2
  STDERR.puts <<USAGE
./record.rb log-filename ./program

excecute the ./program, and records its standard input and output to log-filename.
USAGE
  exit
end

require 'open3'

LogFn = ARGV[0]
Prog  = ARGV[1..-1].join(' ')

Open3.popen3(Prog) {|stdin, stdout, stderr|

  logMutex = Mutex.new

  open(LogFn, 'w') {|fp|
    ti = Thread.new do
      while line = STDIN.gets
        logMutex.synchronize{
          fp.puts "<"+line
        }
        stdin.puts line
        stdin.flush
      end
      stdin.close
    end
    
    to = Thread.new do
      while line = stdout.gets
        logMutex.synchronize{
          fp.puts ">"+line
        }
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
}
