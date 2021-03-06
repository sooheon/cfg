set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

# Remove greeting
set fish_greeting

# PATH
set -x fish_user_paths ~/bin /usr/local/bin /usr/local/sbin ~/.local/bin $fish_user_paths

## add mkl libs to dyld path
set -x DYLD_LIBRARY_PATH /opt/intel/mkl/lib /opt/intel/lib

## select toolchain for swift
set -x PATH /Library/Developer/Toolchains/swift-latest/usr/bin $PATH
set -x TOOLCHAINS swift

# Aliases
alias cfg='/usr/local/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias vi=vim
alias gcc=gcc-9

# ASDF
source /usr/local/opt/asdf/asdf.fish


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/sooheon/.asdf/installs/python/miniconda3-4.3.30/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

set -g fish_user_paths "/usr/local/opt/ncurses/bin" $fish_user_paths
