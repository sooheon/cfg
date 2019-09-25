if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source ~/.vimrc
endif

set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

call plug#begin('~/.vim/bundle')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-rsi'
" Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}

Plug 'guns/vim-clojure-static'
call plug#end()
