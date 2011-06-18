#!/usr/bin/env ruby

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

unless ARGV[0]
  STDERR.puts <<USAGE
#{__FILE__} ./BinaryFileToSubmit [archive_name.tar.gz]
USAGE
end

WorkDir   = '.work/'
BinFn     = ARGV[0]
ArchiveFn = ARGV[1] || 'submit.tar.gz'

sh "git checkout-index -a -f --prefix=ai/#{WorkDir}"
sh "cp install #{WorkDir}"
sh "cp #{BinFn} #{WorkDir}"
sh "cp ../README #{WorkDir}"






