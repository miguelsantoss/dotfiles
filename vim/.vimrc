let mapleader=" "

" set cursorline
set lazyredraw
set nocompatible

syntax on
set ignorecase
set smartcase
set hidden
set hlsearch
set incsearch

set backspace=2
set laststatus=1

set textwidth=80
set colorcolumn=+1
set number

" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<Tab>"
    else
        return "\<C-p>"
    endif
endfunction
inoremap <Tab> <C-r>=InsertTabWrapper()<CR>
inoremap <S-Tab> <C-n>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Always use vertical diffs
set diffopt+=vertical

set mouse=

set tags=.ctags;/
set history=50

" softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

filetype plugin indent on
set wildignore+=*/.git/*,*/tmp/*,*.swp

call plug#begin('~/.vim/plugged')

" Languages
Plug 'vim-ruby/vim-ruby'
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'ap/vim-css-color'
Plug 'janko-m/vim-test'

" linter/syntax checker
Plug 'w0rp/ale'

Plug 'isa/vim-matchit'
Plug 'jiangmiao/auto-pairs'
Plug 'pbrisbin/vim-mkdir'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Plug 'Valloric/YouCompleteMe'

let g:fzf_layout = { 'down': '~40%' }
let g:fzf_buffers_jump = 1

nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>t :Tags<CR>
nnoremap <silent> <leader>T :BTags<CR>
nnoremap <silent> <leader>s :Ag<CR>
nnoremap <silent> <leader>S :Ag!<CR>
nnoremap <silent> <leader>q :Snippets<CR>

nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

Plug 'michaeljsmith/vim-indent-object'

Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'

Plug 'dracula/vim'

" Plug 'itchyny/lightline.vim'
" " Lightline
" let g:lightline = {
"       \ 'colorscheme': 'wombat',
"       \ 'active': {
"       \   'left': [ [ 'mode', 'paste' ],
"       \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
"       \ },
"       \ 'component_function': {
"       \   'gitbranch': 'fugitive#head'
"       \ },
"       \ }

call plug#end()


color dracula

" no bad habits here
inoremap <BS> <Nop>
inoremap <Del> <Nop>
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Keep search results at the center of screen
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
