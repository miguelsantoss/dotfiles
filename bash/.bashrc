export GOPATH=~/Projects/go
export PYENV_ROOT="$HOME/.pyenv"

export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"

export PATH="$PYENV_ROOT/bin:$PATH"
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.rbenv/bin
export PATH=$PATH:$HOME/.fnm
export PATH=$PATH:$HOME/Library/Python/3.7/bin
export PATH=$PATH:$(go env GOPATH)/bin

export RUBY_CONFIGURE_OPTS=--with-jemalloc

eval "$(fnm env --multi)"
eval "$(rbenv init -)"
eval "$(pyenv init -)"

# ls custom colors
export LSCOLORS=ExFxBxDxCxegedabagacad
# Fix support for 256 colors in tmux
[ -n "$TMUX" ] && export TERM=screen-256color-italic

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

## SMARTER TAB-COMPLETION (Readline bindings) ##

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

## SANE HISTORY DEFAULTS ##

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

## BETTER DIRECTORY NAVIGATION ##

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH="."

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion || {
    # if not found in /usr/local/etc, try the brew --prefix location
    [ -f "$(brew --prefix)/etc/bash_completion.d/git-completion.bash" ] && \
        . $(brew --prefix)/etc/bash_completion.d/git-completion.bash
}


function parse_git_branch {
    yellow="\001\033[0;33m\002"
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo -e "$yellow("${ref#refs/heads/}")"
}

set_bash_prompt() {
  if [ "$UID" = 0 ]; then
    #root
    PS1="\[\033[0;31m\]\u\[\033[0;37m\]@\h\[\033[1;37m\] \[\033[0;31m\]\w\$(git_branch)\[\033[0;36m\]λ \[\033[0m\]"
  else
    #non-root
    PS1="\[\033[0;32m\]\w \$(parse_git_branch)\[\033[0;36m\]λ \[\033[0m\]"
    PS1="\[\033[0;32m\]\w \$(parse_git_branch)\[\033[0;36m\]$ \[\033[0m\]"
  fi
}

set_bash_prompt

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

alias ls="ls -hF"
alias ll="ls -lahF"
alias mkdir="mkdir -p"
alias ga="git add"
alias gc="git commit -m"
alias gs="git status"
alias gd="git diff"
alias gf="git fetch"
alias gm="git merge"
alias gr="git rebase"
alias gp="git push"
alias gu="git unstage"
alias gg="git graph"
alias gco="git checkout"
alias gcs="git commit -S -m"
alias gpr="hub pull-request"
alias agg="ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'"
alias tree='tree -CAFa -I="CVS|*.*.package|.svn|.git|.hg|node_modules|bower_components" --dirsfirst'

alias k="kak"
alias v='nvim'
alias e="emacsclient -c"
alias setclip="xclip -selection c"
alias getclip="xclip -selection c -o"
alias ctags="$(brew --prefix)/bin/ctags"
alias be="bundle exec"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"
alias mg="bundle exec rake db:migrate"
alias mt="RAILS_ENV=test bundle exec rake db:drop && RAILS_ENV=test bundle exec rake db:create && RAILS_ENV=test bundle exec rake db:migrate"

alias c="RAILS_ENV=test bundle exec cucumber -p circle"
alias cb="RAILS_ENV=test IN_BROWSER=selenium_chrome bundle exec cucumber -p circle"
alias cs="RAILS_ENV=test SKIP_WEBPACK_IN_TESTS=true bundle exec cucumber -p circle"
alias cf="RAILS_ENV=test FORCE_WEBPACK_BUILD=true bundle exec cucumber -p circle"

### Prompt Colors
# Modified version of @gf3’s Sexy Bash Prompt
# (https://github.com/gf3/dotfiles)
if [[ $COLORTERM = gnome-* && $TERM = xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
	export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
	export TERM=xterm-256color
fi

if tput setaf 1 &> /dev/null; then
	tput sgr0
	if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
		MAGENTA=$(tput setaf 9)
		ORANGE=$(tput setaf 172)
		GREEN=$(tput setaf 190)
		PURPLE=$(tput setaf 141)
	else
		MAGENTA=$(tput setaf 5)
		ORANGE=$(tput setaf 4)
		GREEN=$(tput setaf 2)
		PURPLE=$(tput setaf 1)
	fi
	BOLD=$(tput bold)
	RESET=$(tput sgr0)
else
	MAGENTA="\033[1;31m"
	ORANGE="\033[1;33m"
	GREEN="\033[1;32m"
	PURPLE="\033[1;35m"
	BOLD=""
	RESET="\033[m"
fi

export MAGENTA
export ORANGE
export GREEN
export PURPLE
export BOLD
export RESET

inputcolor="\033[0;37m"
cwdcolor="\033[0;34m"
host_name="\033[1;31m"
branchcolor="\033[0;36m"

# Git branch details
function parse_git_dirty() {
	[[ $(git status 2> /dev/null | tail -n1) != *"working directory clean"* ]] && echo "*"
}
function parse_git_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo -e " ${ref#refs/heads/}"
}

export PS1="\[${MAGENTA}\]\u \[$RESET\]in \[$GREEN\]\w\[$RESET\]\$([[ -n \$(git branch 2> /dev/null) ]] && echo \" on \")\[$PURPLE\]\$(parse_git_branch)\[$RESET\] "
export PS1="\[${host_name}\] λ \[$RESET\]\[$cwdcolor\]\w\[$RESET\]\[$branchcolor\]\$(parse_git_branch)\[$RESET\] "
export PS1="\[$cwdcolor\]\w\[$RESET\]\[$branchcolor\]\$(parse_git_branch)\[$RESET\] "

### Misc

# Only show the current directory's name in the tab
export PROMPT_COMMAND='echo -ne "\033]0;${PWD##*/}\007"'
