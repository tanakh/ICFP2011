#!/usr/bin/env ruby

require 'fileutils'
require 'optparse'

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

$upload = false
ARGV.options {|opt|
  opt.on('-u'){$upload = true}
  opt.parse!
}

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
open("#{WorkDir}/install", 'w') {|fp|
  fp.puts <<SCRIPT
#!/bin/sh
exit 0
SCRIPT
}
open("#{WorkDir}/run", 'w') {|fp|
  fp.puts <<SCRIPT
#!/bin/sh
LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH ./husk $@ 
SCRIPT
}

sh "cp #{BinFn} #{WorkDir}/husk"
sh "chmod 755  #{WorkDir}/install #{WorkDir}/run #{WorkDir}/husk"

sh "cp ../README #{WorkDir}"

if Machine == :arch
  sh "cp /usr/lib/libgmp.so.10 #{WorkDir}"
end

FileUtils.cd(WorkDir) {
  sh "tar czhf #{ArchiveFn} *"
}

sh "cp #{WorkDir}/#{ArchiveFn} ."
sh "sha512sum #{ArchiveFn}"

if $upload
  sh "scp #{ArchiveFn} paraiso-lang.org:/var/www/html/haddock/#{ArchiveFn}"
end

