set fish_greeting ""

set -gx PATH ~/bin /usr/local/bin $PATH

alias ls "ls -hF"
alias ll "ls -lahF"
alias "cd.." "cd ../"
alias mkdir "mkdir -p"
alias ga "git add"
alias gc "git commit -m"
alias gs "git status"
alias gd "git diff"
alias gf "git fetch"
alias gm "git merge"
alias gr "git rebase"
alias gp "git push"
alias gu "git unstage"
alias gg "git graph"
alias gco "git checkout"
alias gcs "git commit -S -m"
alias gpr "hub pull-request"
alias agg "ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'"
alias tree 'tree -CAFa -I "CVS|*.*.package|.svn|.git|.hg|node_modules|bower_components" --dirsfirst'
alias magit 'emacsclient -c -n -e "(progn (magit-status) (delete-other-windows))"'

alias k "kak"
alias v "vim"
alias e "emacsclient -c"
alias setclip "xclip -selection c"
alias getclip "xclip -selection c -o"
alias ctags (brew --prefix)"/bin/ctags"
alias gss "git-fshow"
alias be "bundle exec"
alias rs "bundle exec rails server"
alias rc "bundle exec rails console"
alias mg "bundle exec rake db:migrate"
alias mt "env RAILS_ENV=test bundle exec rake db:drop && env RAILS_ENV=test bundle exec rake db:create && env RAILS_ENV=test bundle exec rake db:migrate"
alias krs "kill -9 "(ps aux | rg 'rails server' | head -n 1 | awk '{print $2}')

alias c "bundle exec cucumber -p circle"
alias cs "SKIP_WEBPACK_IN_TESTS=true bundle exec cucumber -p circle"

alias did "vim +'normal Go' +'r!date' ~/did.txt"

if type -q rbenv
  status --is-interactive; and source (rbenv init -|psub)
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
