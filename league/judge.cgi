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
$poketime = 123


open('register.txt', 'r') {|fp|
  $poketime = fp.gets
  while line = fp.gets
    words = line.split(/\s+/)
    next if words.length < 4
    $ais << AI::new(words[0..-4].join(' '), words[-3], words[-2], words[-1])
  end
}

$fail = false
$errmsg = ''
$update = false 

if $cgi.has_key?('poke') 
  t =  Time.now
  pt = t.to_i
  if pt <= $poketime.to_i + 3
    $errmsg += "Do not poke me so much! <br/><br/>"
  else
    $poketime = pt
    $errmsg = "poked! at #{t}<br/><br/>"
    $update = true
  end
end

if $cgi.has_key?('add') 
  if $cgi['url'] =~ /\s+/ || $cgi['package'] =~ /\s+/
    $fail = true
    $errmsg += <<MSG
URL must not contain spaces<br/><br/>
MSG
  elsif ($cgi['team'].strip.length > 0) && ($cgi['url'].strip.length > 0) && ($cgi['package'].strip.length > 0)
    $ais << AI::new($cgi['team'].strip, digest($cgi['password'].strip),
                    $cgi['url'].strip, $cgi['package'].strip)
    $update = true
  else
    $fail = true
    $errmsg += <<MSG
some fields are missing<br/><br/>
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
Remove requested, but Password Mismatch.<br/><br/>
MSG
        end
      end
      $update = true if flag
      flag
    }
  end
}

if $update 
  open('register.txt', 'w') {|fp|
    begin
      fp.flock(File::LOCK_EX)
      fp.puts $poketime
      $ais.each{|ai|
        fp.puts "#{ai.team} #{ai.passh} #{ai.url} #{ai.package}"
      }
      fp.flock(File::LOCK_UN)
    rescue e
      $errmsg += "#{e.inspect}<br/><br/>"
    end
  }
end
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
            'Takayuki Muranushi (@nushio) from team ' +
            $cgi.a({:href => 'https://github.com/tanakh/ICFP2011'}) {
              'atomically $ save Madoka'
            }
          } + $cgi.p {
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/scoreboard.html'}) {
              'league result (current)'
            }+'&nbsp;'*4+
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/polling_log_current.txt'}) {
              'polling log (current)'
            }+'<br/>'+
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/2/scoreboard.html'}) {
              'league result (last finished)'
            }+'&nbsp;'*4+
            $cgi.a({:href => 'http://www.paraiso-lang.org/Walpurgisnacht/store/polling_log.txt'}) {
              'polling log (last)'
            }+'<br/>'+'<br/>'+
            $cgi.a({:href =>'https://github.com/tanakh/ICFP2011/wiki/League'}) {
              'judge specification (ja)'
            }+'<br/>'+
            $cgi.a({:href =>'http://nushisblogger.blogspot.com/2011/06/yauj-icfpc2011-specification.html'}) {
              'judge specification (en)'
            }+'<br/>'+
            $cgi.a({:href =>'http://nushisblogger.blogspot.com/'}) {
              'YAUJ blog'
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
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'team', :size=>32}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'password' }+
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'password', :name => 'password', :size=>32}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'team website url' }+
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'url', :size=>32}) }
              } + $cgi.tr{
                $cgi.td({:align=>'right'}){ 'submit.tar.gz url' } +
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'text', :name => 'package', :size=>32}) } 
              } + $cgi.tr{
                $cgi.td{} +
                $cgi.td({:align=>'right'}){ $cgi.input({:type => 'submit', :name => 'add', :value => 'submit'}) }
              } + $cgi.tr{
                $cgi.td{'<br/>'} 
              } + $cgi.tr{
                $cgi.td({:colspan=>2}){
                  'remember your password for removal!'
                } 
              } + $cgi.tr{
                $cgi.td{'<br/>'} 
              } + $cgi.tr{
                $cgi.td({:colspan=>2}){
                  'The judge looks at the team list every minute<br/>and starts the match, when' +
                  $cgi.ul {
                    $cgi.li {
                      'the team list is updated,'
                    } + $cgi.li {
                      'previous match is finished,'
                    } + $cgi.li {
                      'and nothing is broken :)'
                    }
                  }
                } 
              } + $cgi.tr{
                $cgi.td({:colspan=>2, :align=>'right'}){
                  'also generate update by pressing:' +$cgi.input({:type => 'submit', :name => 'poke', :value => 'poke!'}) 
                }
              } 
            }
          }
        end
      } # end of CGI
    end
  end
end





