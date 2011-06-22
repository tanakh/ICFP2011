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

$salt=open('/home/web/salt','r').read

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

officialUrl = $cgi.a({:href => 'http://www.icfpcontest.org/' }) {
  'ICFP programming contest 2011'
}
problemUrl = $cgi.a({:href => 'http://www.icfpcontest.org/2011/06/task-description-contest-starts-now.html' }) {
  "The problem"  
}


greetings =  <<MESSAGE
<font color='#ff0000'><b>
#{$errmsg}
</b></font>

Good game, to every participants, and especially to judges, of
#{officialUrl}. #{problemUrl} was really interesting, and I
guess you, as I am, are excited, and cannot wait to tell ones
ideas, learn about others, how well we did, and try out new ideas that
unfortunately we cannot implement in time.  <br/>

So, Here we publish our judge program we used for in-team LTG league
during the ICFP contest. It is provided as it was, to meet the need as
soon as possible. I appologize for those one who cannot run your
program in this judge. If you want to join us, please submit your
teamname, your team URL, and your package.  We are looking forward to
have LTG match with many different strategies and share ideas with you
soon. <br/>

As programming lovers we are, I trust you, but please understand that
when harmful activities are detected, I have to shut down this form of
judge system, and no one will benefit from that. Please include the
source in the package, if possible.  <br/>

MESSAGE



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
          $cgi.h1() {'Yet Another Unofficial Judge for ICFP Contest 2011'
          } + $cgi.p({:align => 'left'}) {
            greetings
          } + $cgi.p({:align => 'right'}) {
            'Takayuki Muranushi (@nushio) from team' +
            $cgi.a({:href => 'https://github.com/tanakh/ICFP2011'}) {
              'atomically $ save Madoka'
            }
          } + $cgi.p {
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/scoreboard.html'}) {
              'league result'
            }+'<br/>'+
            $cgi.a({:href =>'https://github.com/tanakh/ICFP2011/wiki/League'}) {
              'judge specification (ja)'
            }+'<br/>'+
            $cgi.a({:href =>'http://nushisblogger.blogspot.com/2011/06/yauj-icfpc2011-specification.html'}) {
              'judge specification (en)'
            }+'<br/>'+
            $cgi.a({:href =>'http://nushisblogger.blogspot.com/'}) {
              'blog about this'
            }
          } +
            $cgi.table({:border => '1'}) {
            $ais.map {|ai|
              $cgi.tr{
                $cgi.td{
                  $cgi.a({ :href => CGI::escapeHTML(ai.url) }){
                    CGI::escapeHTML(ai.team)
                  } 
                } + $cgi.td{ 
                  packname = CGI::escapeHTML(ai.package) 
                  $cgi.a({ :href => packname }){
                    packname
                  } 

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
                $cgi.td({:align=>'right'}){ 'team website url' }+
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





