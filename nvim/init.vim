if empty(glob(stdpath('data') . '/site/autoload/plug.vim'))
  execute 'silent !curl -fLo' . stdpath('data') . '/site/autoload/plug.vim --create-dirs'
    \ . ' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(stdpath('data') . '/plugged')

Plug 'arcticicestudio/nord-vim'

Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'cohama/lexima.vim'

Plug 'hoob3rt/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim'
Plug 'folke/lsp-colors.nvim'
Plug 'nvim-lua/completion-nvim'
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

Plug 'sbdchd/neoformat'
let g:shfmt_opt="-ci"
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

"快速注释
Plug 'preservim/nerdcommenter'
Plug 'lukas-reineke/indent-blankline.nvim'

"lua
Plug 'tjdevries/nlua.nvim'

call plug#end()

" 基本配置
"init autocmd
autocmd!
" set script encoding
scriptencoding utf-8
syntax enable
set termguicolors
set nocompatible "关闭与vi的兼容模式
set cursorline  " 光标所在的当前行高亮
set number
set fileencodings=utf-8,sjis,euc-jp,latin
set encoding=utf-8
set title
set background=dark
set nobackup
set hlsearch " 搜索时，高亮显示匹配结果
set showcmd
set cmdheight=1
set laststatus=2
set scrolloff=10
set inccommand=split
" Suppress appending <PasteStart> and <PasteEnd> when pasting
set t_BE=

set nosc noru nosm
" Don't redraw while executing macros (good performance config)
set lazyredraw
" Ignore case when searching
set ignorecase
" Be smart when using tabs ;)
set smarttab
" indents
filetype plugin indent on
set shiftwidth=4
set tabstop=4
set expandtab   " Tab 键在不同的编辑器缩进不一致，自动将 Tab 转为空格
set ai "Auto indent
set si "Smart indent
set nowrap "No Wrap lines
set backspace=start,eol,indent
set exrc

set autoread
set clipboard+=unnamed "共享剪切板
set guifont=Fira\ Code\ 16

colorscheme nord

set gcr=n-v-c:ver25-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
