#!/usr/bin/env ruby

require 'fileutils'

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

unless ARGV[0]
  STDERR.puts <<USAGE
#{__FILE__} : creates package, ready for official submission, calculate checksum.

usage:
    #{__FILE__} ./BinaryFileToSubmit [submit.tar.gz]
USAGE
  exit
end

WorkDir   = '.work/'
BinFn     = ARGV[0]
ArchiveFn = ARGV[1] || 'submit.tar.gz'

sh "rm -fr #{WorkDir}"
sh "git checkout-index -a -f --prefix=ai/#{WorkDir}"
sh "cp install #{WorkDir}"
sh "cp #{BinFn} #{WorkDir}"
sh "cp ../README #{WorkDir}"

FileUtils.cd(WorkDir) {
  sh "tar czhf #{ArchiveFn} *"
}

sh "mv #{WorkDir}/#{ArchiveFn} ."
sh "rm -fr #{WorkDir}"
sh "sha512sum #{ArchiveFn}"



