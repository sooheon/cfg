# Prevent duplicates in PATH (https://stackoverflow.com/a/30792333)
typeset -U path
typeset -U fpath

# Add path entries
path+=~/bin:~/.local/bin

export DYLD_LIBRARY_PATH=/opt/intel/lib:/opt/intel/mkl/lib

# Delete with C-w or M-backspace stops at backslash
autoload -U select-word-style
select-word-style bash

# Completion config
setopt auto_cd

# Homebrew completions
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

# asdf support
source /usr/local/opt/asdf/asdf.sh

# Aliases
alias vi=nvim vim=nvim e=emacs
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

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

# Theme
zplugin ice pick'async.zsh' src'pure.zsh'
zplugin light sindresorhus/pure
PURE_PROMPT_SYMBOL='>'
PURE_PROMPT_VICMD_SYMBOL='<'

autoload -Uz compinit && compinit
zplugin cdreplay -q