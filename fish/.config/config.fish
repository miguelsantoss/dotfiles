set fish_greeting ""

set -x -U GOPATH $HOME/go

set -gx PATH ~/bin /usr/local/bin ~/Library/Python/3.7/bin (go env GOPATH)/bin /usr/local/opt/imagemagick@5/bin $PATH

set -x -U OBJC_DISABLE_INITIALIZE_FORK_SAFETY YES
set -x -U RUBY_CONFIGURE_OPTS --with-jemalloc

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
alias gco "git checkout"
alias gpr "hub pull-request"

alias v "nvim"
alias setclip "xclip -selection c"
alias getclip "xclip -selection c -o"
alias ctags (brew --prefix)"/bin/ctags"

alias be "bundle exec"
alias mg "bundle exec rake db:migrate"
alias mt "env RAILS_ENV=test bundle exec rake db:drop db:create db:migrate"

alias c "env RAILS_ENV=test bundle exec cucumber -p circle"
alias cs "env RAILS_ENV=test env SKIP_WEBPACK_IN_TESTS=true bundle exec cucumber -p circle"
alias cf "env RAILS_ENV=test env FORCE_WEBPACK_BUILD=true bundle exec cucumber -p circle"
alias cb "env IN_BROWSER=selenium_chrome env RAILS_ENV=test bundle exec cucumber -p circle"

alias dc "docker-compose"

if type -q rbenv
  status --is-interactive; and source (rbenv init -|psub)

  set -gx PATH ~/.rbenv/bin $PATH
end

if type -q fnm
  fnm env --multi | source
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
