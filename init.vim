" # Plugins Beginning
call plug#begin('~/.vim/plugged/')
    "文件管理
    Plug 'Shougo/denite.nvim'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

    "代码补全
    Plug 'ncm2/ncm2'
    Plug 'ncm2/ncm2-pyclang'
    Plug 'ncm2/ncm2-bufword'
    Plug 'ncm2/ncm2-path'

    "自动括号匹配
    Plug 'jiangmiao/auto-pairs'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    " 支持大多数语言代码高亮
    Plug 'sheerun/vim-polyglot'

    Plug 'junegunn/vim-easy-align'

    "语法检测
    Plug 'w0rp/ale'
    Plug 'scrooloose/nerdtree'
    Plug 'Xuyuanp/nerdtree-git-plugin'
    Plug 'jistr/vim-nerdtree-tabs'
    "搜索文件
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

    Plug 'altercation/vim-colors-solarized'
    "lua
    Plug 'WolfgangMehner/lua-support'

    "HTML Bundles
    Plug 'alvan/vim-closetag'
    Plug 'hail2u/vim-css3-syntax'
    Plug 'gorodinskiy/vim-coloresque'
    Plug 'mattn/emmet-vim'

    "format
    Plug 'Chiel92/vim-autoformat'

    "LSP
    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }
call plug#end()

syntax on
syntax enable

filetype on
filetype plugin on
filetype plugin indent on

" Fundamental settings
set fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,gbk,cp936,latin-1
set fileformat=unix
set fileformats=unix,dos,mac

" Some useful settings
set expandtab         "tab to spaces
set tabstop=4         "the width of a tab
set shiftwidth=4      "the width for indent
set autoread "当文件在外部被修改，自动更新该文件
set smartindent "启用智能对齐方式
set autoindent "自动缩进
set hlsearch "高亮显示搜索结果
set autochdir       "自动切换工作目录
set noundofile "不生成un~文件
set nobackup
set noswapfile

" Lookings
set number           "line number
set cursorline       "hilight the line that the cursor exists in
set cursorcolumn     "hilight the column that the cursor exists in
set nowrap           "no line wrapping
set background=dark
colorscheme solarized

"使得terminal的光标变为细线，而不是默认的粗条。这个在vim的普通模式和插入模式都会生效。
set gcr=n-v-c:ver25-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor

"NERDTree
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

"nerdtree-git-plugin
let g:NERDTreeShowIgnoredStatus = 1
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ 'Ignored'   : '☒',
    \ "Unknown"   : "?"
    \ }


"let g:nerdtree_tabs_open_on_console_startup=1


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

"Emmet
let g:user_emmet_expandabbr_key = '<c-e>'
"let g:user_emmet_leader_key = '<c-e>'
let g:use_emmet_complete_tag = 1

"deoplete
let g:deoplete#enable_at_startup = 1

" Required for operations modifying multiple buffers like rename.
set hidden

" autoformat
au BufWrite * :Autoformat

let g:LanguageClient_serverCommands = {
    \ 'go': ['go-langserver', 'run'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'typescript': ['javascript-typescript-stdio'], 
    \ 'lua': ['lua-lsp'],
    \ 'cpp': ['ccls', '--log-file=/tmp/cq.log'],
    \ 'c': ['ccls', '--log-file=/tmp/cq.log'],
    \ 'python': ['pyls'],
    \ }

" Automatically start language servers.
let g:LanguageClient_loadSettings = 1 " Use an absolute configuration path if you want system-wide settings 
let g:LanguageClient_settingsPath = '/home/jacksoncy/.config/nvim/settings.json'
set completefunc=LanguageClient#complete
set formatexpr=LanguageClient_textDocument_rangeFormatting()
