let mapleader = " "
syntax on
set modeline
set expandtab
set tabstop=4
set shiftwidth=4
set secure
set foldcolumn=0
set number
set splitright
set noshowmode
set noruler
set laststatus=0
set noshowcmd
set cmdheight=1
set mouse=
set mousemodel=extend

imap qw <Esc>
nnoremap <C-t> :vnew <CR>
nnoremap <C-A-t> :new <CR>
nnoremap <C-w> :q <CR>
nnoremap <leader>fs :w<CR>
nnoremap <leader>q :wqa <CR>
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <C-H> <C-W>h
nnoremap <Backspace> :let @/ = ""<CR>
nnoremap <leader><tab> <c-^> <CR>

"-- THEMING --
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

if (has("termguicolors") && $TERM_PROGRAM ==# 'iTerm.app')
  set termguicolors
endif
