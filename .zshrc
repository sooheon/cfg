# Prevent duplicates in PATH (https://stackoverflow.com/a/30792333)
typeset -U path
typeset -U fpath

# Add path entries
path+=~/.cargo/bin:~/bin:~/.local/bin

DYLD_LIBRARY_PATH=/opt/intel/lib:/opt/intel/mkl/lib

# Aliases
alias vi=vim
alias cfg='/usr/local/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Delete with C-w or M-backspace stops at backslash
autoload -U select-word-style
select-word-style bash

# Auto CD when typing dir paths
setopt auto_cd

# Homebrew completions
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

# asdf support
source /usr/local/opt/asdf/asdf.sh

# M-f/M-b fix in IntelliJ IDEA terminal 
# (https://youtrack.jetbrains.com/issue/IDEA-16518)
if [[ "$TERMINAL_EMULATOR" == "JetBrains-JediTerm" ]]; then
    bindkey "ƒ" forward-word   # Option-f
    bindkey "∫" backward-word  # Option-b
    bindkey "∂" delete-word    # Option-d
fi

### Added by Zplugin's installer
source '/Users/sooheon/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin installer's chunk

# Fish like syntax highlighting
# Defer tag >=2 means run after compinit command
zplugin light zsh-users/zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# Fish like autosuggest
zplugin light zsh-users/zsh-autosuggestions
export ZSH_AUTOSUGGEST_USE_ASYNC=true

# Fish like history search
zplugin light zsh-users/zsh-history-substring-search
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# Prompt
zplugin light subnixr/minimal
MNML_USER_CHAR='%%'
MNML_INSERT_CHAR=''
MNML_PROMPT=(mnml_ssh mnml_cwd mnml_status)
MNML_RPROMPT=(mnml_git mnml_pyenv)
MNML_INFOLN=(mnml_err mnml_jobs mnml_files)

autoload -Uz compinit && compinit
zplugin cdreplay -q
