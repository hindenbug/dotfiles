" Load external configuration before anything else
"==============================================================
" VUNDLE STUFF
"==============================================================

if has('vim_starting')
   set nocompatible
   set encoding=utf-8 " Use the only sane encoding choice

   set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'
"==============================================================

" Bundle Config
if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

call neobundle#end()

" load indent file for the current filetype
syntax on             " Enable syntax highlighting
filetype plugin indent on

"==================================================================================================
" THEME SETTINGS
"==================================================================================================
colorscheme base16-railscasts
set background=dark
let base16colorspace=256  " Access colors present in 256 colorspace

highlight clear SignColumn
highlight VertSplit    ctermbg=236
highlight ColorColumn  ctermbg=237
highlight LineNr       ctermbg=236 ctermfg=240
highlight CursorLineNr ctermbg=236 ctermfg=240
highlight CursorLine   ctermbg=236
highlight StatusLineNC ctermbg=238 ctermfg=0
highlight StatusLine   ctermbg=240 ctermfg=12
highlight IncSearch    ctermbg=3   ctermfg=1
highlight Search       ctermbg=1   ctermfg=3
highlight Visual       ctermbg=3   ctermfg=0
highlight Pmenu        ctermbg=240 ctermfg=12
highlight PmenuSel     ctermbg=3   ctermfg=1
highlight SpellBad     ctermbg=0   ctermfg=1

let mapleader = ","
let maplocalleader = "\\"

set mouse=a
set autoread
set binary
set cinoptions=:0,(s,u0,U1,g0,t0
"set completeopt=menuone,preview
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮,trail:·
set notimeout
set noeol
set numberwidth=3
set winwidth=83
set textwidth=100
"set ofu=syntaxcomplete#Complete
set clipboard+=unnamed
set history=1000

if has('persistent_undo')
  set undofile                " So is persistent undo ...
  set undolevels=1000         " Maximum number of changes that can be undone
  set undoreload=10000        " Maximum number lines to save for undo on a buffer reload
endif

set splitright     " Puts new vsplit windows to the right of the current
set splitbelow     " Puts new split windows to the bottom of the current
set modelines=2    " Check 2 lines of files for commands
set autoindent     " Continue previous line's indent by default
set showmode       " Show mode in last line
set showcmd        " Show visual selection size in last line
set hidden         " Don't unload abandoned buffers
set cursorline     " Highlight the entire line the cursor is on
hi CursorLine term=bold cterm=bold guibg=Grey40 ctermfg=grey
set ttyfast        " Assume a fast terminal connection
set backspace=indent,eol,start " Sane edge case behavior for Backspace key
set relativenumber " Use relative line numbering
set number
set laststatus=2   " Show a status line for all windows always
set showbreak=↪    " Use this character to indicate wrapping
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
set guioptions-=T
set t_Co=256
set scrolloff=8    "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=0
set hlsearch       " highlight search terms
set incsearch      " show search matches as you type

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=-1
set timeoutlen=1000
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
set lazyredraw
set iskeyword-=_

au VimEnter * RainbowParenthesesToggle
augroup rainbow_parentheses
  autocmd Syntax clojure RainbowParenthesesActivate
  autocmd Syntax clojure RainbowParenthesesLoadRound
  autocmd Syntax clojure RainbowParenthesesLoadSquare
  autocmd Syntax clojure RainbowParenthesesLoadBraces
augroup END

let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^fnk', '^dfnk']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

let g:easytree_use_plus_and_minus = 1
let g:easytree_show_line_numbers = 0

let g:ctrlp_working_path_mode = 0  " Search from current directory instead of project root
let g:ctrlp_max_files = 0          " Set no max file limit

"let g:indentobject_meaningful_indentation = ["haml", "sass", "python", "yaml", "markdown", "ruby"]
let g:indent_guides_enable_on_vim_startup = 0
let g:LargeFile=5
let g:ruby_path = system('echo $HOME/.rbenv/shims')

let g:gitgutter_enabled = 1

"Ack / ag (the_silver_searcher)
let g:ackprg = 'ag --nogroup --nocolor --column'

" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap \ :Ag<SPACE>

" Adjust viewports to the same size
map <Leader>= <C-w>=
nmap <Tab> <C-w>w

" Leave Ex Mode
nnoremap Q <nop>

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

set guicursor=n-v-c:ver20
set guicursor+=i:hor10

if has("gui_running")
    set guifont=Monaco\ for\ Powerline:h11
    set guioptions-=T " no toolbar
    set guioptions-=m " no menus
    set guioptions-=r " no scrollbar on the right
    set guioptions-=R " no scrollbar on the right
    set guioptions-=l " no scrollbar on the left
    set guioptions-=b " no scrollbar on the bottom
    set guioptions=aiA
   set mouse=v
endif

if !has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=1000
    augroup END
endif

" =================================================================================================
" FORMATTING
" =================================================================================================

" Set up a keymapping from <Leader>df to a function call.
" (Note the function doesn't need to be defined beforehand.)
" Run this mapping silently. That is, when I call this mapping,
" don't bother showing "call DiffToggle()" on the command line.
nnoremap <silent> <Leader>df :call DiffToggle()<CR>

" Define a function called DiffToggle.
" The ! overwrites any existing definition by this name.
function! DiffToggle()
    " Test the setting 'diff', to see if it's on or off.
    " (Any :set option can be tested with &name.
    " See :help expr-option.)
    if &diff
        diffoff
    else
        diffthis
    endif
:endfunction


" Removes trailing spaces
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

"autocmd!
autocmd FileType ruby,haml,slim,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
autocmd FileType python set sw=4 sts=4 et
autocmd BufRead *.md set ai formatoptions=tcroqn2 comments=n:&gt;
autocmd BufRead *.markdown set ai formatoptions=tcroqn2 comments=n:&gt;
autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd BufWritePre * :call TrimWhiteSpace() " strip trailing whitespace

"Format xml files
"au FileType xml exe ":silent 1,$!xmllint --format --recover - 2>/dev/null"
autocmd BufNewFile,BufRead *.slim set syntax=slim

au BufNewFile * set noeol


" ==================================================================================================
" The Silver Searcher
" ==================================================================================================

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

"====================================================================================================
" CUSTOM BIDINGS
"====================================================================================================

nmap <C-e> :EasyTreeToggle<CR>

" Keep NERDTree window fixed between multiple toggles

" stop arrow keys.
"noremap <left> <nop>
"noremap <up> <nop>
"noremap <down> <nop>
"noremap <right> <nop>

" :W is bound to :w.
command! W :w

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" bind L to grep word under cursor
nnoremap L :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

nmap <Leader>bi :source ~/.vimrc<cr>:NeoBundleInstall<cr>
map <Leader>rs :vsp <C-r>#<cr><C-w>w
nmap <space> i_<esc>r

"Edit another file in the same directory as the current file
" uses expression to extract path from current file's path
map <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>s :split <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>v :vnew <C-R>=expand("%:p:h") . '/'<CR>

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

" format the entire file
nmap <leader>fef ggVG=
nmap <leader>h :nohlsearch<cr>

nnoremap ; :
nnoremap <leader>\ :Eval<CR>

"==================================================================================================
" FORMATTING
"==================================================================================================

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

set nocompatible


"=========================================================================
" CUSTOM LIGHTLINE CONFIG
" ========================================================================

let g:lightline = {
      \ 'colorscheme': 'powerline',
      \ 'mode_map': { 'c': 'NORMAL' },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
      \ },
      \ 'component_function': {
      \   'modified': 'MyModified',
      \   'readonly': 'MyReadonly',
      \   'fugitive': 'MyFugitive',
      \   'filename': 'MyFilename',
      \   'fileformat': 'MyFileformat',
      \   'filetype': 'MyFiletype',
      \   'fileencoding': 'MyFileencoding',
      \   'mode': 'MyMode',
      \ },
      \ 'separator': {'left': '⮀', 'right': '⮂' } ,
      \ 'subseparator': { 'left': '⮁', 'right': '⮃' }
      \ }

function! MyModified()
  return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
  return &ft !~? 'help' && &readonly ? '⭤' : ''
endfunction

function! MyFilename()
  return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let _ = fugitive#head()
    return strlen(_) ? '⭠ '._ : ''
  endif
  return ''
endfunction

function! MyFilename()
  return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
  let fname = expand('%:t')
  return fname == '__Tagbar__' ? 'Tagbar' :
        \ fname == 'ControlP' ? 'CtrlP' :
        \ fname == '__Gundo__' ? 'Gundo' :
        \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
        \ fname =~ 'NERD_tree' ? 'NERDTree' :
        \ &ft == 'unite' ? 'Unite' :
        \ &ft == 'vimfiler' ? 'VimFiler' :
        \ &ft == 'vimshell' ? 'VimShell' :
        \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction