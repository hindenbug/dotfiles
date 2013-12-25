set nocompatible
filetype on
filetype off

" Load external configuration before anything else
"==============================================================
" VUNDLE STUFF
"==============================================================

if has('vim_starting')
   set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

 call neobundle#rc(expand('~/.vim/bundle/'))

 " Let NeoBundle manage NeoBundle
 NeoBundleFetch 'Shougo/neobundle.vim'
"==============================================================


" Bundle Config
if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif


"==================================================================================================
" THEME SETTINGS
"==================================================================================================
colorscheme molokai


let mapleader = ","
let maplocalleader = "\\"


" load indent file for the current filetype
set nocompatible      " We're running Vim, not Vi!
syntax on             " Enable syntax highlighting

filetype plugin indent on

if !has('gui')
  "set term=$TERM          " Make arrow and other keys work
endif

set scrolloff=0    " Set 5 lines to the cursor - when moving vertically
set mouse=a
set autoread
set binary
set cinoptions=:0,(s,u0,U1,g0,t0
set completeopt=menuone,preview
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮,trail:␣
set notimeout
set noeol
set numberwidth=3
set winwidth=83
set textwidth=100
"set foldlevelstart=0
"set foldmethod=syntax
set ofu=syntaxcomplete#Complete
set clipboard+=unnamed
set history=1000
if has('persistent_undo')
  set undofile                " So is persistent undo ...
  set undolevels=1000         " Maximum number of changes that can be undone
  set undoreload=10000        " Maximum number lines to save for undo on a buffer reload
endif
highlight clear SignColumn      " SignColumn should match background
highlight clear LineNr          " Current line number row will have same background color in relative mode
set splitright                  " Puts new vsplit windows to the right of the current
set splitbelow                  " Puts new split windows to the bottom of the current
set background=dark
set encoding=utf-8 " Use the only sane encoding choice
set modelines=2    " Check 2 lines of files for commands
set autoindent     " Continue previous line's indent by default
set showmode       " Show mode in last line
set showcmd        " Show visual selection size in last line
set hidden         " Don't unload abandoned buffers
set cursorline     " Highlight the entire line the cursor is on
set ttyfast        " Assume a fast terminal connection
set backspace=indent,eol,start " Sane edge case behavior for Backspace key
set relativenumber " Use relative line numbering
set number
set laststatus=2   " Show a status line for all windows always
set matchtime=3    " Auto-highlight '%' match for 0.3s
set showbreak=↪    " Use this character to indicate wrapping
set autowrite      " Write modified files on certain commands
set shiftround     " Round indents to multiple of shiftwidth
set title          " Update the (terminal) window title
set linebreak      " Break lines at opportune characters
set nospell
set smartindent
set copyindent     " copy the previous indentation on autoindenting
set nowritebackup
set noswapfile
set nobackup
set guioptions+=LlRrb
set guioptions-=LlRrb
set guioptions-=T
set t_Co=256
set scrolloff=8    "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=0
set shiftround     " use multiple of shiftwidth when indenting with '<' and '>'
set hlsearch       " highlight search terms
set incsearch      " show search matches as you type

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=1
set visualbell t_vb=    " Turn off flashing

" Command line completion:
set wildchar=<TAB>      " Character to start command line completion

" First list the possible completions.
" Then complete to longest matching string.
" Finally start cycling between completions (starts wildmenu mode).
set wildmode=list,longest,full

" Ignore these directories
set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,.DS_Store,*.aux,*.out,*.toc,tmp,*.scssc
set wildmenu            " Enhanced command line completion mode


" Tabs, spaces, wrapping
set formatoptions+=tcrqnb
set formatoptions-=o
set ignorecase
set smartcase
set showmatch
set gdefault
set scrolloff=3
set sidescroll=1
set sidescrolloff=10
set virtualedit+=block
set nofsync

let g:LargeFile=5
let g:ruby_path = system('echo $HOME/.rbenv/shims')

" Set no max file limit
let g:ctrlp_max_files = 0

" Search from current directory instead of project root
let g:ctrlp_working_path_mode = 0
let g:indentobject_meaningful_indentation = ["haml", "sass", "python", "yaml", "markdown", "ruby"]
let g:indent_guides_enable_on_vim_startup = 0
let g:airline_left_sep='›'  " Slightly fancier than '>'
let g:airline_right_sep='‹' " Slightly fancier than '<'

let g:rubycomplete_buffer_loading = 0
let g:rubycomplete_classes_in_global = 1


let g:airline_powerline_fonts = 1
" Broken down into easily includeable segments
set statusline=%<%f\                     " Filename
set statusline+=%w%h%m%r                 " Options
set statusline+=%{fugitive#statusline()} " Git Hotness
set statusline+=\ [%{&ff}/%Y]            " Filetype
set statusline+=\ [%{getcwd()}]          " Current dir
set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info

set lazyredraw

let g:airline_enable_branch     = 1
let g:airline_enable_syntastic  = 1
let g:airline_theme             = 'powerlineish'
" vim-powerline symbols
let g:airline_left_sep          = '⮀'
let g:airline_left_alt_sep      = '⮁'
let g:airline_right_sep         = '⮂'
let g:airline_right_alt_sep     = '⮃'
let g:airline_branch_prefix     = '⭠'
let g:airline_readonly_symbol   = '⭤'
let g:airline_linecolumn_prefix = '⭡'

" Adjust viewports to the same size
map <Leader>= <C-w>=

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'


" =================================================================================================
" Ruby stuff
" =================================================================================================

augroup myfiletypes
  autocmd!
  autocmd FileType slim,coffee,ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

augroup myfiletypes
  " Clear old autocmds in group
  autocmd!
  " autoindent with two spaces, always expand tabs
  autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
  autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  autocmd FileType * set ai sw=2 sts=2 ts=2 et
  autocmd FileType python,html,xml,markdown set ai sw=4 sts=4 et
  "autocmd User Rails set tabstop=2 shiftwidth=2 softtabstop=2 expandtab
augroup END


" ==================================================================================================
" The Silver Searcher
" ==================================================================================================

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

"Ack / ag (the_silver_searcher)
let g:ackprg = 'ag --nogroup --nocolor --column'


"====================================================================================================
" CUSTOM BIDINGS
"====================================================================================================

nmap <C-e> :NERDTreeToggle<CR>
" Keep NERDTree window fixed between multiple toggles
set winfixwidth

" stop arrow keys.
"noremap <left> <nop>
"noremap <up> <nop>
"noremap <down> <nop>
"noremap <right> <nop>

" Yank from current cursor position to end of line
map Y y$

" clear highlight after search
noremap <silent><Leader>/ :nohls<CR>

" better ESC
inoremap <C-k> <Esc>

" :W is bound to :w.
command! W :w

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" Fast saving and closing current buffer without closing windows displaying the
" buffer
nmap <leader>wq :w!<cr>:Bclose<cr>

" bind L to grep word under cursor
nnoremap L :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

nmap <Leader>bi :source ~/.vimrc<cr>:BundleInstall<cr>
map <Leader>gs :Gstatus<CR>
map <Leader>rs :vsp <C-r>#<cr><C-w>w
map <Leader>ss ds)i <esc>:w<cr>
map <Leader>w <C-w>w
nmap <space> i_<esc>r

"Edit another file in the same directory as the current file
" uses expression to extract path from current file's path
map <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>s :split <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>v :vnew <C-R>=expand("%:p:h") . '/'<CR>

map <C-s> <esc>:w<CR>
imap <C-s> <esc>:w<CR>
map <C-t> <esc>:tabnew<CR>

" Emacs-like beginning and end of line.
imap <c-e> <c-o>$
imap <c-a> <c-o>^

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

command! Q q " Bind :Q to :q
command! Qall qall

" Disable Ex mode
map Q <Nop>

" Let's be reasonable, shall we?
nmap k gk
nmap j gj

" Jump to the next row on long lines
map <Down> gj
map <Up>   gk
nnoremap j gj
nnoremap k gk

" more natural movement with wrap on
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Bubbling lines
"nmap <C-Up> [e
"nmap <C-Down> ]e
"vmap <C-Up> [egv
"vmap <C-Down> ]egv


" format the entire file
nmap <leader>fef ggVG=

" Open new buffers
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

" Tab between buffers
noremap <tab> <c-w><c-w>

" Switch between last two buffers
nnoremap <leader><leader> <C-^>

nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

inoremap <C-s> <esc>:w<cr>a
nnoremap <C-s> :w<cr>a

nnoremap ; :

" Easy buffer navigation
noremap <leader>bp :bprevious<cr>
noremap <leader>bn :bnext<cr>


nmap <leader># :call NERDComment(0, "invert")<cr>
vmap <leader># :call NERDComment(0, "invert")<cr>


"==================================================================================================
" FORMATTING
"==================================================================================================

" Remove trailing whitespace on save for ruby files.
au BufWritePre *.rb :%s/\s\+$//e
autocmd BufWritePre * :%s/\s\+$//e

" Save when losing focus
au FocusLost    * :silent! wall
"
" When vimrc is edited, reload it
autocmd! BufWritePost vimrc source ~/.vimrc

augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:␣
    au InsertLeave * :set listchars+=trail:␣
augroup END


"Format xml files
au FileType xml exe ":silent 1,$!xmllint --format --recover - 2>/dev/null"

" When loading text files, wrap them and don't split up words.
au BufNewFile,BufRead *.txt setlocal wrap
au BufNewFile,BufRead *.txt setlocal lbr

" Turn on spell-checking in markdown and text.
au BufRead,BufNewFile *.md,*.txt setlocal spell

" Automatic formatting
autocmd BufWritePre *.rb :%s/\s\+$//e
autocmd BufWritePre *.haml :%s/\s\+$//e
"autocmd BufWritePre *.html :%s/\s\+$//e
autocmd BufWritePre *.scss :%s/\s\+$//e
"autocmd BufWritePre *.slim :%s/\s\+$//e


au BufNewFile * set noeol

" No show command
autocmd VimEnter * set nosc

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE (thanks Gary Bernhardt)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <Leader>n :call RenameFile()<cr>


set list lcs=tab:\|\

au BufNewFile,BufReadPost *.html setl shiftwidth=2 tabstop=2 softtabstop=2 expandtab
au BufNewFile,BufReadPost *.slim setl shiftwidth=2 tabstop=2 softtabstop=2 expandtab
au BufNewFile,BufReadPost *.coffee setl shiftwidth=2 tabstop=2 softtabstop=2 expandtab


