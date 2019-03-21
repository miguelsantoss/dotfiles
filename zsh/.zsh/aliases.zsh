alias ls="ls -hF"
alias ll="ls -lahF"
alias "cd.."="cd ../"
alias mkdir="mkdir -p"
alias cp="cp -r"
alias scp="scp -r"

alias ga="git add"
alias gc="git commit -m"
# alias gs="git status"
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
alias rm="rm -r"
alias setclip="xclip -selection c"
alias getclip="xclip -selection c -o"
alias gw="cd /mnt/Storage/Downloads"
alias gy="cd /mnt/Storage/Yarr"
alias gE="cd /run/media/migui/DOLAN/"

alias rec='ffmpeg -f x11grab -s 1366x768 -an -i :0.0 -c:v libvpx -b:v 5M -crf 10 -quality realtime -y -loglevel quiet'
alias fastrec='ffmpeg -f x11grab -s 1366x768 -an -r 25 -i :0.0 -c:v libvpx -b:v 5M -crf 10 -quality realtime -y -loglevel quiet'

alias pu="sudo xbps-install -Su"
alias xi="xbps-install -S"

alias pu="sudo pacman -Syu"
alias xi="pacman -S"

alias se="sudoedit"
alias xc="xclip -selection c"
alias sudo="sudo "
alias gss="git-fshow"
