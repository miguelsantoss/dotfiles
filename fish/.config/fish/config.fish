set fish_greeting ""

set -x EDITOR nvim

set -gx PATH ~/bin /usr/local/bin ~/Library/Python/3.7/bin $PATH
set -gx PATH /usr/local/opt/mongodb-community@4.4/bin $PATH

set -gx JAVA_HOME (/usr/libexec/java_home -v11)
set -gx JAVA_HOME /Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home

# graalvm
# set -gx GRAALVM_HOME /Library/Java/JavaVirtualMachines/graalvm-ce-java16-21.2.0/Contents/Home/
# set -gx GRAALVM_HOME /Library/Java/JavaVirtualMachines/graalvm-ce-java17-21.3.0/Contents/Home/
set -gx GRAALVM_HOME /Library/Java/JavaVirtualMachines/graalvm-ce-java11-21.2.0/Contents/Home/
set -gx PATH $GRAALVM_HOME/bin $PATH
set -gx JAVA_HOME $GRAALVM_HOME

# set -gx NVM_DIR $HOME/.nvm

set -gx DUMP_DIR $HOME/src/dump/

if type -q go
  set -gx PATH (go env GOPATH)/bin $PATH
  set -x -U GOPATH $HOME/go
end

if type -q fnm
  fnm env | source
end

if status --is-interactive
  abbr -a -g ls "ls -hF"
  abbr -a -g ll "ls -lahF"
  
  abbr -a -g "cd.." "cd ../"
  abbr -a -g mkdir "mkdir -p"
  
  abbr -a -g ga "git add"
  abbr -a -g gc "git commit -m"
  abbr -a -g gs "git status"
  abbr -a -g gd "git diff"
  abbr -a -g gf "git fetch"
  abbr -a -g gm "git merge"
  abbr -a -g gr "git rebase"
  abbr -a -g gp "git push"
  abbr -a -g gu "git unstage"
  abbr -a -g gco "git checkout"
  abbr -a -g gpr "hub pull-request"
  
  abbr -a -g v "nvim"
  abbr -a -g vim "nvim"
  abbr -a -g setclip "xclip -selection c"
  abbr -a -g getclip "xclip -selection c -o"
  
  abbr -a -g dc "docker-compose"
  
  abbr -a -g m "make"
  abbr -a -g mdu "make docker-start"
  abbr -a -g mds "make docker-stop"
  abbr -a -g pd "make playdev PLAYDEV_ARGS=\"-mem 4096 -J-Xmx4096m -java-home $GRAALVM_HOME \""
  abbr -a -g pdd "make playdevdebug PLAYDEV_ARGS=\"-mem 4096 -J-Xmx4096m -java-home $GRAALVM_HOME\""
  abbr -a -g wo "make playdevworkeronly PLAYDEV_ARGS=\"-mem 4096 -J-Xmx4096m -java-home $GRAALVM_HOME\""
  
  abbr -a -g t "timew start"
  abbr -a -g st "timew stop"
end

if type -q fasd
  alias a "fasd -a"
  alias s "fasd -si"
  alias d "fasd -d"
  alias f "fasd -f"
  alias sd "fasd -sid"
  alias sf "fasd -sif"
  alias z "fasd_cd -d"
  alias zz "fasd_cd -d -i"
end
