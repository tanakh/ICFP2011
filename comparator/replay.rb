#!/usr/bin/env ruby


if ARGV.length < 1
  STDERR.puts <<USAGE
./replay.rb log-filename

behave exactly as is recorded in log-filename
USAGE
  exit
end

open(ARGV[0], 'r') {|fp|
  while log = fp.gets
    cmd = log[0]
    line = log[1..-1]
    case cmd
    when ">"
      puts line
      STDOUT.flush
    when "<"
      line2 = STDIN.gets
      unless line == line2
        throw "input mismatch : expected '#{line[0..-2]}' : actual '#{line2[0..-2]}'"
      end
    end
  end
}

