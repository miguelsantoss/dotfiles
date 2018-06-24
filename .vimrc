let g:python3_host_prog = '/usr/bin/python3'
let mapleader=" "

set termguicolors
set cursorline
set lazyredraw
set nocompatible

syntax on
set ignorecase
set smartcase
set hidden
set hlsearch
set incsearch

set backspace=2
set laststatus=2

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+1

" Numbers
set number
set relativenumber

" " Tab completion
" " will insert tab at beginning of line,
" " will use completion if not at beginning
" set wildmode=list:longest,list:full
" function! InsertTabWrapper()
"     let col = col('.') - 1
"     if !col || getline('.')[col - 1] !~ '\k'
"         return "\<Tab>"
"     else
"         return "\<C-p>"
"     endif
" endfunction
" inoremap <Tab> <C-r>=InsertTabWrapper()<CR>
" inoremap <S-Tab> <C-n>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Autocomplete with dictionary words when spell check is on
set complete+=kspell

" Always use vertical diffs
set diffopt+=vertical

highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=steelblue
set guicursor=n-v-c-i:block-Cursor
set guicursor+=n-v-c-i:blinkon0
set guifont=Monaco:h12
set guioptions=
set mouse=
autocmd FocusLost * redraw!
let g:has_async = v:version >= 800 || has('nvim')

let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'
set viminfo='1000,f1,:1000,/1000

set tags=.ctags;/
set autowrite
set history=50

" softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

filetype plugin indent on
set wildignore+=*/.git/*,*/tmp/*,*.swp

augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile .{jscs,jshint,eslint}rc set filetype=json

  " ALE linting events
  if g:has_async
    set updatetime=1000
    let g:ale_lint_on_text_changed = 0
    autocmd CursorHold * call ale#Lint()
    autocmd CursorHoldI * call ale#Lint()
    autocmd InsertEnter * call ale#Lint()
    autocmd InsertLeave * call ale#Lint()
  else
    echoerr "The thoughtbot dotfiles require NeoVim or Vim 8"
  endif
augroup END

call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
let g:deoplete#enable_at_startup = 1

" Languages
Plug 'vim-ruby/vim-ruby'
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'ap/vim-css-color'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
let g:UltiSnipsExpandTrigger="<tab>"

Plug 'janko-m/vim-test'

" linter/syntax checker
Plug 'w0rp/ale'

Plug 'isa/vim-matchit'
Plug 'jiangmiao/auto-pairs'
Plug 'pbrisbin/vim-mkdir'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

let g:fzf_layout = { 'down': '~40%' }
let g:fzf_buffers_jump = 1

let g:rg_command = '
  \ rg --column --line-number --no-heading --fixed-strings --ignore-case
  \ --no-ignore --hidden --follow --color "always"
  \ --glob "!{.git,node_modules,vendor}/*" --glob "!*.o" '

nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>F :History<CR>
nnoremap <silent> <leader>gf :call fzf#run({'source': 'fd --type f --hidden --follow --exclude .git', 'sink': 'e', 'down': '40%', 'options':'--query '.expand('<cword>')})<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>l :BLines<CR>
nnoremap <silent> <leader>L :Lines<CR>
nnoremap <silent> <leader>t :Tags<CR>
nnoremap <silent> <leader>T :BTags<CR>
nnoremap <silent> <leader>s :Rg<CR>
nnoremap <silent> <leader>S :Rg!<CR>
nnoremap <silent> <leader>q :Snippets<CR>

nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

imap <C-x><C-f> <plug>(fzf-complete-file-ag)
imap <C-x><C-l> <plug>(fzf-complete-line)

nnoremap <silent> K :call SearchWordWithAg()<CR>
vnoremap <silent> K :call SearchVisualSelectionWithAg()<CR>
nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

function! SearchWordWithAg()
  execute 'Ag' expand('<cword>')
endfunction

function! SearchVisualSelectionWithAg() range
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  let old_clipboard = &clipboard
  set clipboard&
  normal! ""gvy
  let selection = getreg('"')
  call setreg('"', old_reg, old_regtype)
  let &clipboard = old_clipboard
  execute 'Ag' selection
endfunction

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

" Distraction free
Plug 'junegunn/goyo.vim'
Plug 'amix/vim-zenroom2'
Plug 'junegunn/limelight.vim'

let g:limelight_default_coefficient = 0.7
let g:limelight_conceal_ctermfg = 238

nnoremap <silent> <C-Z> :Goyo<cr>
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

Plug 'ctrlpvim/ctrlp.vim'
Plug 'nixprime/cpsm'

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_match_func = {'match': 'cpsm#CtrlPMatch'}

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
  set grepformat=%f:%l:%c:%m
  let g:ackprg = 'rg --vimgrep --no-heading --no-messages'
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
elseif executable('ag')
  set grepprg=ag\ --vimgrep\ --ignore=\"**.min.js\"
  set grepformat=%f:%l:%c:%m,%f:%l:%m
  let g:ctrlp_use_caching = 0
elseif executable('ag')
else
  let g:ctrlp_clear_cache_on_exit = 0
endif

Plug 'michaeljsmith/vim-indent-object'

Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
" wiki/notes
Plug 'vimwiki/vimwiki'

Plug 'crusoexia/vim-monokai'
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'joshdick/onedark.vim'
Plug 'dracula/vim'
let g:quantum_italics=1
let g:onedark_terminal_italics=1

Plug 'itchyny/lightline.vim'
" Lightline
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ }

call plug#end()

colorscheme dracula

" highlight trailing white spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

command! WQ wq
command! Wq wq
command! W w
command! Q q

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" bad habits
inoremap <BS> <Nop>
inoremap <Del> <Nop>
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc
nnoremap <C-n> :call NumberToggle()<cr>

" Keep search results at the center of screen
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
