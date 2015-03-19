# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#
export PATH="$HOME/.bin:$PATH"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

for file in ~/.{path,zshrc,exports,aliases,functions}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file; 

source '/usr/local/share/chruby/chruby.sh'
source '/usr/local/share/chruby/auto.sh'

# Customize to your needs...
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

export PYENV_ROOT="${HOME}/.pyenv"
export PYTHONSTARTUP=~/.pythonrc
export PATH=~/bin:$PATH
export KEYTIMEOUT=1
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
export PGHOST=localhost
export PYTHONDONTWRITEBYTECODE=true

eval "$(fasd --init zsh-ccomp zsh-ccomp-install posix-alias zsh-hook)"
if [ -d "${PYENV_ROOT}" ]; then
  export PATH="${PYENV_ROOT}/bin:${PATH}"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

man() {
    env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}
