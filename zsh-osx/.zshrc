export ZSH_AUTOSUGGEST_USE_ASYNC=1

# source .zsh files
source $HOME/.zsh/aliases.zsh
source $HOME/.zsh/autocompletion.zsh
source $HOME/.zsh/colored-man-pages.zsh
source $HOME/.zsh/git.zsh
source $HOME/.zsh/history.zsh
source $HOME/.zsh/keychain.zsh
source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/syntax.zsh

# vim mode
bindkey -v
bindkey '^R' history-incremental-search-backward

eval "$(rbenv init -)"
source $(brew --prefix)/etc/profile.d/z.sh

em () {
  open -a $(brew --prefix)/bin/emacs
}

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

fzf_vim () {
  vim $(fzf)
}

zle -N fzf_vim
bindkey '^P' fzf_vim

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
  hash=$(git log --color=never --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | awk '{print $1}')
  echo $hash | xclip
  git showtool $hash
}


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
