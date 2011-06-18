#!/usr/bin/env ruby

def sh(cmd)
  STDERR.puts cmd
  system(cmd)
end

WorkDir = '.work/'

"git checkout-index -a -f --prefix=ai/#{WorkDir}"







