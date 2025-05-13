# Set PATH to include user's private bin if it exists
[[ -d "$HOME/bin" ]] && PATH="$HOME/bin:$PATH"

# Set PATH to include user's local bin if it exists
[[ -d "$HOME/.local/bin" ]] && PATH="$HOME/.local/bin:$PATH"

# Source the cargo env if it exists
[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# Source Neovim
[[ -d "/opt/nvim-linux-x86_64/bin" ]] && PATH="/opt/nvim-linux-x86_64/bin:$PATH"

typeset -U path
path=(${(s.:.)PATH})
export PATH="${(j.:.)path}"
