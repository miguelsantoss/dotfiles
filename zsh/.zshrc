# source .zsh files
for config ($HOME/.zsh/*.zsh) source $config

bindkey '^R' history-incremental-search-backward

em () {
  open -a $(brew --prefix)/bin/emacs
}

if which rbenv &>/dev/null; then
  eval "$(rbenv init -)"
fi

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

bindkey '^f' autosuggest-accept

note (){
  local notes_dir="$HOME/notes"
  case "$1" in
    c)
      cd "$notes_dir"
      ;;
    l)
      ls "$notes_dir"
      ;;
    *)
      pushd "$notes_dir"
      if [ $# -eq 0 ]; then
        v "$1"
      else
        v "$1".md
      fi
      popd
  esac
}

fzf_log() {
  hash=$(git log --color=never --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" | fzf | awk '{print $1}')
  echo $hash | xclip
  git showtool $hash
}


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
