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

EtcIssue = open('/etc/issue', 'r').read

Machine = if EtcIssue.index('Arch')
            :arch
          else
            :compatible
          end

sh "rm -fr #{WorkDir}"
sh "git checkout-index -a -f --prefix=ai/#{WorkDir}"
sh "mv #{WorkDir}/ai #{WorkDir}/src"
sh "cp install #{WorkDir}"
sh "cp #{BinFn} #{WorkDir}/husk"
sh "cp ../README #{WorkDir}"

if Machine == :arch
  "cp /usr/lib/libgmp.so.10 #{WorkDir}"
end

FileUtils.cd(WorkDir) {
  sh "tar czhf #{ArchiveFn} *"
}

sh "mv #{WorkDir}/#{ArchiveFn} ."
sh "sha512sum #{ArchiveFn}"



