set nocompatible
set encoding=utf-8 " Use the only sane encoding choice

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

" load indent file for the current filetype
syntax on             " Enable syntax highlighting
filetype plugin indent on

"==================================================================================================
" THEME SETTINGS
"==================================================================================================
colorscheme grb256
set background=dark
let base16colorspace=256  " Access colors present in 256 colorspace
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

let g:nerdtree_tabs_open_on_gui_startup = 0  " Auto open nerd tree on startup
let g:nerdtree_tabs_focus_on_files = 1       " Focus in the main content window

let g:syntastic_check_on_open=0
let g:syntastic_echo_current_error=0
let g:syntastic_auto_jump=0
let g:syntastic_auto_loc_list=0

" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

let g:ctrlp_working_path_mode = 0  " Search from current directory instead of project root
let g:ctrlp_max_files = 0          " Set no max file limit

"let g:indentobject_meaningful_indentation = ["haml", "sass", "python", "yaml", "markdown", "ruby"]
let g:indent_guides_enable_on_vim_startup = 0
"let g:rubycomplete_buffer_loading = 1
"let g:rubycomplete_classes_in_global = 1
let g:LargeFile=5
let g:ruby_path = system('echo $HOME/.rbenv/shims')

"let g:calendar_google_calendar = 1
"let g:calendar_google_task = 1
let g:gitgutter_enabled = 1

" Adjust viewports to the same size
map <Leader>= <C-w>=

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

if has("gui_running")
    set guifont=Monaco\ for\ Powerline
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

"autocmd!
autocmd FileType ruby,haml,slim,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
autocmd FileType python set sw=4 sts=4 et
autocmd BufRead *.md set ai formatoptions=tcroqn2 comments=n:&gt;
autocmd BufRead *.markdown set ai formatoptions=tcroqn2 comments=n:&gt;
autocmd BufWritePre * :%s/\s\+$//e " strip trailing whitespace

" augroup trailing
"     au!
"     au InsertEnter * :set listchars-=trail:·
"     au InsertLeave * :set listchars+=trail:·
" augroup END

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

" Fast saving and closing current buffer without closing windows displaying the
" buffer
"nmap <leader>wq :w!<cr>:Bclose<cr>

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

nnoremap ; :

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
      \ 'separator': { 'left': '⮀', 'right': '⮂' },
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

function! CtrlPMark()
  if expand('%:t') =~ 'ControlP'
    call lightline#link('iR'[g:lightline.ctrlp_regex])
    return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
          \ , g:lightline.ctrlp_next], 0)
  else
    return ''
  endif
endfunction

let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction

" function! MyMode()
"   return winwidth(0) > 60 ? lightline#mode() : ''
" endfunction
"
