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

## Antidote setup

[[ -e $HOME/.antidote ]] ||
	git clone --depth=1 https://github.com/mattmc3/antidote.git $HOME/.antidote

source $HOME/.antidote/antidote.zsh
source <(antidote init)

# Antidote packages

antidote bundle zsh-users/zsh-autosuggestions
antidote bundle zsh-users/zsh-syntax-highlighting

antidote load

## Starship init

eval "$(starship init zsh)"

## Variables

export EDITOR=nvim

# Aliases

alias vi="nvim"
alias oldvi="\vi"

alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias cpwezlinux="cp /mnt/c/Users/Paulo/.wezterm.lua ~/"
alias cpwezwin="cp ~/.wezterm.lua /mnt/c/Users/Paulo/"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
