for config ($HOME/.zsh/*.zsh) source $config
bindkey -v
bindkey '^R' history-incremental-search-backward

eval `ssh-agent -s` > /dev/null
# ssh-add

# source ~/.zsh/z.sh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax/syntax.zsh

export TERM=xterm-256color

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden . $HOME'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_ALT_C_COMMAND='cd ~ && fd --hidden --no-ignore --ignore-case --type d'

fc() {
  hash=$(git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |  fzf | awk '{print $1}')
  git checkout $hash
}
