#!/usr/local/bin/ruby -Ku

require 'cgi'
require 'digest/sha2'

$cgi = CGI::new('html4Tr')


class AI
  def initialize(team, passh, url, package)
    @team = team
    @passh = passh
    @url  = url
    @package = package
  end
  attr_accessor :team, :passh, :url, :package
end

$salt=open('../hentai/salt','r').read

def digest(str)
  Digest::SHA512.hexdigest($salt + str)
end

$ais = []


open('register.txt', 'r') {|fp|
  while line = fp.gets
    words = line.split(/\s+/)
    next if words.length < 4
    $ais << AI::new(words[0..-4].join(' '), words[-3], words[-2], words[-1])
  end
}

$fail = false
$errmsg = ''

if $cgi.has_key?('add') 
  if $cgi['url'] =~ /\s+/ || $cgi['package'] =~ /\s+/
    $fail = true
    $errmsg += <<MSG
URL must not contain spaces<br><br>
MSG
  elsif ($cgi['team'].strip.length > 0) && ($cgi['url'].strip.length > 0) && ($cgi['package'].strip.length > 0)
    $ais << AI::new($cgi['team'].strip, digest($cgi['password'].strip),
                    $cgi['url'].strip, $cgi['package'].strip)
  else
    $fail = true
    $errmsg += <<MSG
some fields are missing<br><br>
MSG
  end
  
end


$cgi.params.each{|k,v|
  if k[0..5] == 'remove'
    md5 = k[6..-1]
    $ais.reject!{|ai|
      flag = false
      if md5==digest(ai.team)      
        if ai.passh == digest($cgi['password'].strip)
          flag = true
        else
          $fail = true
          $errmsg += <<MSG
Remove requested, but Password Mismatch.<br><br>
MSG
        end
      end
      flag
    }
  end
}

open('register.txt', 'w') {|fp|
  $ais.each{|ai|
    fp.puts "#{ai.team} #{ai.passh} #{ai.url} #{ai.package}"
  }
}

$cgi.out() do
  $cgi.html() do
    $cgi.head() do
      $cgi.title {
        "Submit Your Soul!"
      }
    end +
    $cgi.body() do
      $cgi.form({:action => 'index.cgi', :method => 'post'}) {
        $cgi.center() do
          $cgi.h1() {'Yet Another Unofficial Judge Registeration Form'
          } + $cgi.p({:align => 'left'}) {(<<STR)} + 
<font color="#ff0000"><b>
#{$errmsg}
</b></font>

Here we publish our judge program we used during the ICFP contest for in-team LTG league.
If you want to join us, please submit your teamname, your team URL, and your package.
We are looking forward to have LTG match with many different strategies and share ideas with you soon. 
<br>
As programming lovers we are, we trust you,
but for security concerns, we will reserve the right to examine and reject your package.
Please include the source in the package, if possible. 
<br>
At the moment, we are working on upgrading the judge system for generating game records etc...
As soon as we finish on the update, we will start the league.
STR
          $cgi.p {
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/scoreboard.html'}) {
              'league result'
            }
          } +
            $cgi.table({:border => '1'}) {
            $ais.map {|ai|
              $cgi.tr{
                $cgi.td{
                  $cgi.a({ :href => ai.url }){
                    ai.team 
                  } 
                } + $cgi.td{ 
                  ai.package 
                } + $cgi.td{ 
                  remove_tag = 'remove'+digest(ai.team)
                  $cgi.input({:type => 'submit', :name => remove_tag, :value => 'remove'}) 
                }
              }
            }.join("\n")
          } + $cgi.p {
            $cgi.table {
              $cgi.tr{
                $cgi.td({:align=>'right'}){ 'team name' } + 
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'team'}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'password' }+
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'password', :name => 'password'}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'team homupage url' }+
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'url'}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'submit.tar.gz url' } +
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'package'}) } 
              } +    
              $cgi.td{} +
              $cgi.td({:align=>'right'}){ $cgi.input({:type => 'submit', :name => 'add', :value => 'submit'}) }
            } 
          } + $cgi.p {
            'remember your password for removal!'
          }
        end
      } # end of CGI
    end
  end
end





