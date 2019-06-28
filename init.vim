if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" # Plugins Beginning
call plug#begin()
"文件管理
Plug 'Shougo/denite.nvim'

"自动括号匹配
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" 支持大多数语言代码高亮
Plug 'sheerun/vim-polyglot'

Plug 'junegunn/vim-easy-align'

"语法检测
Plug 'w0rp/ale'

Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }

" 快速注释
Plug 'scrooloose/nerdcommenter'

Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'

"lua
Plug 'WolfgangMehner/lua-support'

"HTML Bundles
Plug 'gorodinskiy/vim-coloresque'

"bash
Plug 'vim-scripts/bash-support.vim'

"go
Plug 'fatih/vim-go'

Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-match-highlight'
Plug 'ncm2/float-preview.nvim'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-vim'
Plug 'ncm2/ncm2-syntax'
Plug 'Shougo/neco-syntax'
Plug 'ncm2/ncm2-ultisnips'
Plug 'SirVer/ultisnips'
Plug 'fgrsnau/ncm2-aspell'

"format
Plug 'sbdchd/neoformat'

Plug 'terryma/vim-multiple-cursors'

Plug 'Yggdroot/indentLine'

Plug 'google/vim-searchindex'

" vsc
Plug 'airblade/vim-gitgutter'

Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

call plug#end()

" Some useful settings
set expandtab         "tab to spaces
set tabstop=4         "the width of a tab
set shiftwidth=4      "the width for indent
set smartindent "启用智能对齐方式
set autochdir       "自动切换工作目录
set noundofile "不生成un~文件
set nobackup
set noswapfile
set cindent
set nocp

" Lookings
set number           "line number
set cursorline       "hilight the line that the cursor exists in
set cursorcolumn     "hilight the column that the cursor exists in
set nowrap           "no line wrapping
set termguicolors
set modeline            " Enable modeline.
colorscheme gruvbox
set background=dark

set guifont=Fira\ Code\ 16

"使得terminal的光标变为细线，而不是默认的粗条。这个在vim的普通模式和插入模式都会生效。
set gcr=n-v-c:ver25-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor

"format
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

"Defx
call defx#custom#option('_', {
            \ 'winwidth': 30,
            \ 'split': 'vertical',
            \ 'direction': 'topleft',
            \ 'show_ignored_files': 0,
            \ 'buffer_name': '',
            \ 'toggle': 1,
            \ 'resume': 1
            \ })

" airline
let g:airline_powerline_fonts=1
let g:airline_theme='powerlineish'
let g:airline#extensions#tabline#enabled = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline#extensions#tabline#buffer_idx_mode = 1

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANTE: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect

" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_serverCommands = {
            \ 'javascript': ['typescript-language-server'],
            \ 'typescript': ['typescript-language-server'],
            \ 'javascript.jsx': ['typescript-language-server'],
            \ 'sh': ['bash-language-server', 'start'],
            \ 'lua': ['lua-lsp'],
            \ 'python': ['pyls'],
            \ 'go': ['gopls'],
            \ 'yaml': ['node', 'yaml-language-server/out/server/src/server.js', '--stdio'],
            \ 'c': ['ccls', '--log-file=/tmp/cc.log'],
            \ 'cpp': ['clangd'],
            \ }

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

let g:ale_linters = {
            \ 'sh': ['language_server'],
            \ 'go': ['golangci-lint'],
            \ }
