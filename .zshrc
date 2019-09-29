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
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

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

# History
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
setopt BANG_HIST              # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY       # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY          # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_IGNORE_SPACE      # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS      # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY            # Don't execute immediately upon history expansion.
setopt HIST_BEEP              # Beep when accessing nonexistent history.

autoload -Uz compinit && compinit
zplugin cdreplay -q
