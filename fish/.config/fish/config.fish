# Start X at login
# if status --is-login
#   if test -z "$DISPLAY" -a (fgconsole) = 1
#     exec startx
#   end
# end

set -gx PATH $HOME/.rbenv/bin $HOME/.rbenv/shims $HOME/bin /usr/local/bin $HOME/.fzf/bin $PATH

set -gx FZF_DEFAULT_COMMAND 'find $HOME -type f'
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -gx FZF_ALT_C_COMMAND 'find $HOME -type d'

set fish_greeting ""

# longer path prompt
set -U fish_prompt_pwd_dir_length 0

# aliases
alias ll="ls -lahF --color=auto"
alias "cd.."="cd .."

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

alias ag="ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'"
alias rock="ncmpcpp"
alias disks='echo "╓───── m o u n t . p o i n t s"; echo "╙────────────────────────────────────── ─ ─ "; lsblk -a; echo ""; echo "╓───── d i s k . u s a g e"; echo "╙────────────────────────────────────── ─ ─ "; df -h;'
alias record="ffmpeg -f x11grab -s 1366x768 -an -r 16 -loglevel quiet -i :0.0 -b:v 5M -y" #pass a filename
alias v="vim"
alias e="emacsclient -c"
alias setclip="xclip -selection c"
alias getclip="xclip -selection c -o"

alias rec='ffmpeg -f x11grab -s 1366x768 -an -i :0.0 -c:v libvpx -b:v 5M -crf 10 -quality realtime -y -loglevel quiet'
alias fastrec='ffmpeg -f x11grab -s 1366x768 -an -r 25 -i :0.0 -c:v libvpx -b:v 5M -crf 10 -quality realtime -y -loglevel quiet'

alias pu="sudo xbps-install -Suv"
alias xi="xbps-install -S"
alias se="sudoedit"
alias xc="xclip -selection c"
alias sudo="sudo "
alias gss="git-fshow"
