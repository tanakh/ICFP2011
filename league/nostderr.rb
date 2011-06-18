#!/usr/bin/env ruby


require 'open3'

Prog  = ARGV.join(' ')

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
    stdout.close
  end

  te = Thread.new do
    while line = stderr.gets
      # fuuin
    end
    stderr.close
  end
    
  ti.join
  to.join
  te.join
}
