" Stolen from github.com/skwp/dotfiles

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
set laststatus=2
set encoding=utf-8
set t_Co=256

" Use pathogen to easily modify the runtime path to include all plugins under
" " the ~/.vim/bundle directory
filetype off                    " force reloading *after* pathogen loaded
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'rking/ag.vim'
Plugin 'toyamarinyon/vim-swift'
Plugin 'tangphillip/SunburstVIM'
Plugin 'altercation/vim-colors-solarized'
Plugin 'nanotech/jellybeans.vim'
Plugin 'L9'
Plugin 'kien/ctrlp.vim'
call vundle#end()
filetype plugin indent on       " enable detection, plugins and indenting in one step

" ================ General Config ====================

set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set noerrorbells                " no more error bells
set gcr=a:blinkon0              "Disable cursor blink
set autoread                    "Reload files changed outside vim
set ttyfast                     "Optimize for fast terminal connections
set clipboard=unnamed           "Use OS clipboard by default
set modeline
set modelines=4
set cursorline
set tags=.git/tags,./tags,~/tags

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden

"turn on syntax highlighting
syntax enable
set background=dark
set nocursorcolumn
set nocursorline
syntax sync minlines=256
let g:is_bash = 1

" Airline configuration
let g:airline_powerline_fonts = 1
let g:airline_detect_paste = 1
let g:airline_theme='wombat'

" Change the mapleader from \ to ,
let mapleader=","
let maplocalleader="\\"

" ================ Colors ===========================
" colorscheme ir_black
" colorscheme grb256
colorscheme jellybeans
let g:solarized_termcolors=256
highlight clear SignColumn

" ================ Key bindings =====================

" Remap j and k to act as expected when used on long, wrapped, lines
nnoremap j gj
nnoremap k gk

" Easy window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
nmap <leader>s <C-w>s
nmap <leader>v <C-w>v

" Quick save and exit
nmap <leader>q <C-w>q
nmap <leader>w :update<CR>

" Quick yanking to the end of the line
nnoremap Y y$

" Clears the search register
nnoremap <silent> <leader>/ :nohlsearch<CR>
" Dynamic search highlight
set incsearch

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null" Sudo to write

" ================ Turn Off Swap Files ==============

set noswapfile
set nobackup
set nowb

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.

if v:version >= 730
    silent !mkdir ~/.vim/backups > /dev/null 2>&1
    set undofile                " keep a persistent backup file
    set undodir=~/.vim/backups
endif

" ================ Indentation ======================

set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set noexpandtab

nmap <leader>m :set expandtab tabstop=2 softtabstop=2 shiftwidth=2<CR>
nmap <leader>p :set expandtab tabstop=4 softtabstop=4 shiftwidth=4<CR>
nmap <leader>l :set expandtab tabstop=3 softtabstop=3 shiftwidth=3<CR>
nmap <leader>k :set noexpandtab tabstop=8 softtabstop=8 shiftwidth=8<CR>

autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype c setlocal noexpandtab ts=8 sts=8 sw=8
autocmd Filetype php setlocal noexpandtab ts=4 sts=4 sw=4
autocmd Filetype haskell setlocal expandtab
" autocmd Syntax * syn match ExtraWhitespace /\s\+$\| \+\ze\t/

filetype plugin on
filetype indent on

" Display tabs and trailing spaces visually
set list listchars=tab:\»\ ,trail:·
set wrap         "Don't wrap lines
set linebreak    "Wrap lines at convenient points
highlight SpecialKey ctermfg=59
" set nolist       "List disables linebreak

" ================ Folds ============================

set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Completion =======================

set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

" ================ Scrolling ========================

set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescroll=1
