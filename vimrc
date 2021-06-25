if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" # Plugins Beginning
call plug#begin()

"自动括号匹配
Plug 'LunarWatcher/auto-pairs'

"modeline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" 支持大多数语言代码高亮
Plug 'sheerun/vim-polyglot'

Plug 'voldikss/vim-translate-me'

" 浮窗终端
Plug 'voldikss/vim-floaterm'
let g:floaterm_position = 'center'
let g:floaterm_keymap_new = '<F7>'
let g:floaterm_keymap_kill = '<F8>'

Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

" 快速注释
Plug 'preservim/nerdcommenter'
" <leader>cc   加注释
" <leader>cu   解开注释
" <leader>c<space>  加上/解开注释, 智能判断
" <leader>cy   先复制, 再注解(p可以进行黏贴)
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not 
let g:NERDToggleCheckAllLines = 1

" 主题
Plug 'arcticicestudio/nord-vim'

" lua
Plug 'tbastos/vim-lua'
Plug 'spacewander/openresty-vim'
Plug 'chr4/nginx.vim'

" bash
Plug 'WolfgangMehner/bash-support'

" yaml
Plug 'stephpy/vim-yaml'

"format
Plug 'sbdchd/neoformat'
let g:shfmt_opt="-ci"
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

Plug 'terryma/vim-multiple-cursors'
let g:multi_cursor_use_default_mapping=0
" Default mapping
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

Plug 'Yggdroot/indentLine'

" search
Plug 'liuchengxu/vim-clap'

" vsc
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

call plug#end()

" Some useful settings
set expandtab         "tab to spaces
set tabstop=4         "the width of a tab
set shiftwidth=4      "the width for indent
set smartindent "启用智能对齐方式
set autochdir       "自动切换工作目录
set noundofile "不生成un~文件
set nobackup
set nowritebackup
set noswapfile
set cindent
set nocp

set encoding=utf-8
set hidden
set shortmess+=c
set signcolumn=yes

set guifont=Fira\ Code\ 16

" 自动加载修改过的文件
au FocusGained * :checktime

" Lookings
set number           "line number
set cursorline       "hilight the line that the cursor exists in
set cursorcolumn     "hilight the column that the cursor exists in
set nowrap           "no line wrapping
set termguicolors
set modeline            " Enable modeline.

" colorscheme onedark
colorscheme nord

"使得terminal的光标变为细线，而不是默认的粗条。这个在vim的普通模式和插入模式都会生效。
set gcr=n-v-c:ver25-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor

" airline
let g:airline_powerline_fonts=1
" let g:airline_theme='powerlineish'
let g:airline_theme = 'nord_minimal'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#coc#enabled = 1
let g:airline#extensions#ale#enabled = 1
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
