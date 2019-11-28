if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" # Plugins Beginning
call plug#begin()

"自动括号匹配
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'

"modeline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" 支持大多数语言代码高亮
Plug 'sheerun/vim-polyglot'

Plug 'junegunn/vim-easy-align'

Plug 'voldikss/vim-translate-me'

" 浮窗终端
Plug 'voldikss/vim-floaterm'
noremap  <silent> <F12>           :FloatermToggle<CR>
noremap! <silent> <F12>           <Esc>:FloatermToggle<CR>
tnoremap <silent> <F12>           <C-\><C-n>:FloatermToggle<CR>
let g:floaterm_position = 'center'

"语法检测
Plug 'dense-analysis/ale'

Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
" Defx git
Plug 'kristijanhusak/defx-git'
Plug 'ryanoasis/vim-devicons'
Plug 'kristijanhusak/defx-icons'
let g:defx_icons_enable_syntax_highlight = 1

Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }

" 快速注释
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-commentary'

"主题
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
Plug 'kaicataldo/material.vim'

"lua
Plug 'WolfgangMehner/lua-support'
Plug 'spacewander/openresty-vim'

"bash
Plug 'vim-scripts/bash-support.vim'

"markdown
Plug 'plasticboy/vim-markdown'
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_follow_anchor = 1
let g:vim_markdown_math = 1
let g:vim_markdown_strikethrough = 1

" cmake
Plug 'vhdirk/vim-cmake'

"go
Plug 'fatih/vim-go'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1

let g:go_def_reuse_buffer = 1
let g:go_def_mode = 'gopls'
let g:go_info_mode = 'gopls'
let g:go_fmt_command = 'goimports'
let g:go_metalinter_command = 'golangci-lint'
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']

let g:go_auto_type_info = 1         " auto show the type info of cusor
let g:go_doc_keywordprg_enabled = 1 " map K to :GoDoc, use coc-action-doHover instead

Plug 'honza/vim-snippets'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"format
Plug 'sbdchd/neoformat'

Plug 'terryma/vim-multiple-cursors'

Plug 'Yggdroot/indentLine'

" fzf
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'google/vim-searchindex'

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
set noswapfile
set cindent
set nocp

" 自动加载修改过的文件
set autoread
au FocusGained * :checktime

" Lookings
set number           "line number
set cursorline       "hilight the line that the cursor exists in
set cursorcolumn     "hilight the column that the cursor exists in
set nowrap           "no line wrapping
set termguicolors
set modeline            " Enable modeline.
" colorscheme onedark
let g:material_terminal_italics = 1
let g:material_theme_style = 'palenight'
colorscheme material
" set background=dark

set guifont=Fira\ Code\ 16

"使得terminal的光标变为细线，而不是默认的粗条。这个在vim的普通模式和插入模式都会生效。
set gcr=n-v-c:ver25-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor

"为python和shell等添加注释
autocmd FileType python,shell,coffee set commentstring=#\ %s
"修改注释风格
autocmd FileType java,c,cpp set commentstring=//\ %s

"注释
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


"format
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

" airline
let g:airline_powerline_fonts=1
let g:airline_theme='powerlineish'
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

" Required for operations modifying multiple buffers like rename.
set hidden

set nowritebackup

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" coc extensions
let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-marketplace',
  \ 'coc-angular',
  \ 'coc-tabnine',
  \ 'coc-dictionary',
  \ 'coc-emoji',
  \ 'coc-emmet',
  \ 'coc-eslint',
  \ 'coc-explorer',
  \ 'coc-git',
  \ 'coc-highlight',
  \ 'coc-html',
  \ 'coc-lists',
  \ 'coc-json',
  \ 'coc-pairs',
  \ 'coc-post',
  \ 'coc-python',
  \ 'coc-snippets',
  \ 'coc-syntax',
  \ 'coc-tag',
  \ 'coc-template',
  \ 'coc-tsserver',
  \ 'coc-vimlsp',
  \ 'coc-yank',
  \ 'coc-yaml',
  \ 'coc-snippets',
  \ 'coc-sh',
  \ 'coc-sql',
  \ 'coc-word'
\ ]
if matchstr(&rtp, 'coc.nvim') != ''
  call coc#add_extension()
endif

" Ale
let g:ale_linters = {
      \ 'sh': ['language_server'],
      \ 'python': ['mypy'],
      \ 'go': ['golangci-lint'],
      \ 'javascript': ['eslint'],
      \ }
let g:ale_go_golangci_lint_package = 1
let g:ale_go_golangci_lint_options = '--fast -E golint --exclude-use-default=false'
"自定义error和warning图标
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚡'
"在vim自带的状态栏中整合ale
let g:ale_statusline_format = ['✗ %d', '⚡ %d', '✔ OK']

" virtual text, conflicts with coc-git
let g:ale_virtualtext_cursor = 1
let g:ale_virtualtext_prefix = ' > '
hi link ALEError ALEErrorSign
hi link ALEWarning ALEWarningSign

" Defx
call defx#custom#option('_', {
            \ 'winwidth': 30,
            \ 'columns': 'indent:git:icons:filename:type',
            \ 'split': 'vertical',
            \ 'direction': 'topleft',
            \ 'show_ignored_files': 0,
            \ 'buffer_name': '',
            \ 'toggle': 1,
            \ 'resume': 1
            \ })

autocmd FileType defx call s:defx_my_settings()
function! s:defx_my_settings() abort
  setl nospell
  setl signcolumn=no
  setl nonumber
  nnoremap <silent><buffer><expr> <CR>
  \ defx#is_directory() ?
  \ defx#do_action('open_or_close_tree') :
  \ defx#do_action('drop',)
  nmap <silent><buffer><expr> <2-LeftMouse>
  \ defx#is_directory() ?
  \ defx#do_action('open_or_close_tree') :
  \ defx#do_action('drop',)
  nnoremap <silent><buffer><expr> s defx#do_action('drop', 'split')
  nnoremap <silent><buffer><expr> v defx#do_action('drop', 'vsplit')
  nnoremap <silent><buffer><expr> t defx#do_action('drop', 'tabe')
  nnoremap <silent><buffer><expr> o defx#do_action('open_tree')
  nnoremap <silent><buffer><expr> O defx#do_action('open_tree_recursive')
  nnoremap <silent><buffer><expr> C defx#do_action('copy')
  nnoremap <silent><buffer><expr> P defx#do_action('paste')
  nnoremap <silent><buffer><expr> M defx#do_action('rename')
  nnoremap <silent><buffer><expr> D defx#do_action('remove_trash')
  nnoremap <silent><buffer><expr> A defx#do_action('new_multiple_files')
  nnoremap <silent><buffer><expr> U defx#do_action('cd', ['..'])
  nnoremap <silent><buffer><expr> . defx#do_action('toggle_ignored_files')
  nnoremap <silent><buffer><expr> <Space> defx#do_action('toggle_select')
  nnoremap <silent><buffer><expr> R defx#do_action('redraw')
endfunction

" Defx git
call defx#custom#column('git', 'indicators', {
  \ 'Modified'  : '✹',
  \ 'Staged'    : '✚',
  \ 'Untracked' : '✭',
  \ 'Renamed'   : '➜',
  \ 'Unmerged'  : '═',
  \ 'Ignored'   : '☒',
  \ 'Deleted'   : '✖',
  \ 'Unknown'   : '?'
  \ })
let g:defx_git#column_length = 0
hi def link Defx_filename_directory NERDTreeDirSlash
hi def link Defx_git_Modified Special
hi def link Defx_git_Staged Function
hi def link Defx_git_Renamed Title
hi def link Defx_git_Unmerged Label
hi def link Defx_git_Untracked Tag
hi def link Defx_git_Ignored Comment

" Prettier for Lua
function PrettierLuaCursor()
  let save_pos = getpos(".")
  %! prettier --stdin --parser=lua
  call setpos('.', save_pos)
endfunction
" define custom command
command PrettierLua call PrettierLuaCursor()
" format on save
autocmd BufwritePre *.lua PrettierLua
