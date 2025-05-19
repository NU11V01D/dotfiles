HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1500
setopt extendedglob
setopt auto_cd
bindkey -v

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

zstyle :compinstall filename '/home/paulinux/.zshrc'

autoload -Uz compinit
compinit

# End of lines added by compinstall

source $HOME/antigen.zsh

antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

eval "$(starship init zsh)"

export EDITOR=nvim

alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias copywez="cp /mnt/c/Users/Paulo/.wezterm.lua ~/"
