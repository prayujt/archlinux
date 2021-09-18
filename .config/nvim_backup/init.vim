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

call plug#begin('~/.config/nvim/plugged')
    Plug 'kaicataldo/material.vim', {'branch': 'main'}
    Plug 'vim-airline/vim-airline'
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'preservim/nerdtree'
	"Plug 'ryanoasis/vim-devicons'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'evanleck/vim-svelte', {'branch': 'main'}
call plug#end()

"-- THEMING --
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

if (has("termguicolors") && $TERM_PROGRAM ==# 'iTerm.app')
  set termguicolors
endif

"let g:material_theme_style = 'darker'
"hi Normal       ctermbg=NONE guibg=NONE
"hi SignColumn   ctermbg=235 guibg=#262626
"hi LineNr       ctermfg=grey guifg=grey ctermbg=NONE guibg=NONE
"hi CursorLineNr ctermbg=NONE guibg=NONE ctermfg=178 guifg=#d7af00
"let g:airline#extensions#tabline#enabled = 1

" -- NVIM Tree --
" let g:nvim_tree_side = 'left' "left by default
" let g:nvim_tree_width = 40 "30 by default, can be width_in_columns or 'width_in_percent%'
" let g:nvim_tree_ignore = [ '.git', 'node_modules', '.cache' ] "empty by default
" let g:nvim_tree_gitignore = 1 "0 by default
" let g:nvim_tree_auto_open = 1 "0 by default, opens the tree when typing `vim $DIR` or `vim`
" let g:nvim_tree_auto_close = 1 "0 by default, closes the tree when it's the last window
" let g:nvim_tree_auto_ignore_ft = [ 'startify', 'dashboard' ] "empty by default, don't auto open tree on specific filetypes.
" let g:nvim_tree_quit_on_open = 1 "0 by default, closes the tree when you open a file
" let g:nvim_tree_follow = 1 "0 by default, this option allows the cursor to be updated when entering a buffer
" let g:nvim_tree_indent_markers = 1 "0 by default, this option shows indent markers when folders are open
" let g:nvim_tree_hide_dotfiles = 1 "0 by default, this option hides files and folders starting with a dot `.`
" let g:nvim_tree_git_hl = 1 "0 by default, will enable file highlight for git attributes (can be used without the icons).
" let g:nvim_tree_highlight_opened_files = 1 "0 by default, will enable folder and file icon highlight for opened files/directories.
" let g:nvim_tree_root_folder_modifier = ':~' "This is the default. See :help filename-modifiers for more options
" let g:nvim_tree_tab_open = 1 "0 by default, will open the tree when entering a new tab and the tree was previously open
" let g:nvim_tree_auto_resize = 0 "1 by default, will resize the tree to its saved width when opening a file
" let g:nvim_tree_disable_netrw = 0 "1 by default, disables netrw
" let g:nvim_tree_hijack_netrw = 0 "1 by default, prevents netrw from automatically opening when opening directories (but lets you keep its other utilities)
" let g:nvim_tree_add_trailing = 1 "0 by default, append a trailing slash to folder names
" let g:nvim_tree_group_empty = 1 " 0 by default, compact folders that only contain a single folder into one node in the file tree
" let g:nvim_tree_lsp_diagnostics = 1 "0 by default, will show lsp diagnostics in the signcolumn. See :help nvim_tree_lsp_diagnostics
" let g:nvim_tree_disable_window_picker = 1 "0 by default, will disable the window picker.
" let g:nvim_tree_hijack_cursor = 0 "1 by default, when moving cursor in the tree, will position the cursor at the start of the file on the current line
" let g:nvim_tree_icon_padding = ' ' "one space by default, used for rendering the space between the icon and the filename. Use with caution, it could break rendering if you set an empty string depending on your font.
" let g:nvim_tree_update_cwd = 1 "0 by default, will update the tree cwd when changing nvim's directory (DirChanged event). Behaves strangely with autochdir set.
" let g:nvim_tree_window_picker_exclude = {
    " \   'filetype': [
    " \     'packer',
    " \     'qf'
    " \   ],
    " \   'buftype': [
    " \     'terminal'
    " \   ]
    " \ }
" Dictionary of buffer option names mapped to a list of option values that
" indicates to the window picker that the buffer's window should not be
" selectable.
" let g:nvim_tree_special_files = { 'README.md': 1, 'Makefile': 1, 'MAKEFILE': 1 } " List of filenames that gets highlighted with NvimTreeSpecialFile
" let g:nvim_tree_show_icons = {
    " \ 'git': 1,
    " \ 'folders': 0,
    " \ 'files': 0,
    " \ 'folder_arrows': 0,
    " \ }
"If 0, do not show the icons for one of 'git' 'folder' and 'files'
"1 by default, notice that if 'files' is 1, it will only display
"if nvim-web-devicons is installed and on your runtimepath.
"if folder is 1, you can also tell folder_arrows 1 to show small arrows next to the folder icons.
"but this will not work when you set indent_markers (because of UI conflict)

" default will show icon by default if no icon is provided
" default shows no icon by default
" let g:nvim_tree_icons = {
    " \ 'default': '',
    " \ 'symlink': '',
    " \ 'git': {
    " \   'unstaged': "✗",
    " \   'staged': "✓",
    " \   'unmerged': "",
    " \   'renamed': "➜",
    " \   'untracked': "★",
    " \   'deleted': "",
    " \   'ignored': "◌"
    " \   },
    " \ 'folder': {
    " \   'arrow_open': "",
    " \   'arrow_closed': "",
    " \   'default': "",
    " \   'open': "",
    " \   'empty': "",
    " \   'empty_open': "",
    " \   'symlink': "",
    " \   'symlink_open': "",
    " \   },
    " \   'lsp': {
    " \     'hint': "",
    " \     'info': "",
    " \     'warning': "",
    " \     'error': "",
    " \   }
    " \ }

" nnoremap <leader>ff :NvimTreeToggle<CR>
" nnoremap <leader>fr :NvimTreeRefresh<CR>
" nnoremap <leader>fe :NvimTreeFindFile<CR>
" NvimTreeOpen and NvimTreeClose are also available if you need them

" a list of groups can be found at `:help nvim_tree_highlight`
" highlight NvimTreeFolderIcon guibg=blue


" -- NERDTree --
let NERDTreeShowHidden=0
let NERDTreeMapUpdir = '<S-TAB>'
let NERDTreeQuitOnOpen = 1
let NERDTreeMapChangeRoot = '<TAB>'
let NERDTreeMapToggleHidden = '.'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
nnoremap <leader>ff :NERDTreeToggle<CR>

autocmd BufEnter * lcd %:p:h
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |

" Coc server
nmap <leader>r <Plug>(coc-rename)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

inoremap <expr> <Return> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

nnoremap <silent> <C-Space> :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Snippets
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

" Use <leader>x for convert visual selected code to snippet
xmap <leader>s <Plug>(coc-convert-snippet)

" FZF
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

nnoremap <leader>s :Rg<CR>

" Goyo
" autocmd VimEnter * :Goyo
