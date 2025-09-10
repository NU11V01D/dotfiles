HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1500
setopt extendedglob
setopt auto_cd
bindkey -e

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit

# End of lines added by compinstall

if [[ ! -f "$HOME/antigen.zsh" ]]; then
  curl -L git.io/antigen > "$HOME/antigen.zsh"
fi

source $HOME/antigen.zsh

antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

eval "$(starship init zsh)"

export EDITOR=nvim

alias vi="nvim"
alias oldvi="\vi"

alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias copywez="cp /mnt/c/Users/Paulo/.wezterm.lua ~/"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
