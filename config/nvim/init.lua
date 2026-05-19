-- ============================================================================
-- Leader Keys (must be set before plugins)
-- ============================================================================
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

-- Disable unused built-in plugins
vim.g.loaded_gzip = 1
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_tutor = 1
vim.g.loaded_zipPlugin = 1

-- ============================================================================
-- Editor Settings
-- ============================================================================
local opt = vim.opt

-- Indentation
local indent = 4
opt.shiftwidth = indent
opt.tabstop = indent
opt.softtabstop = indent
opt.smartindent = true
opt.expandtab = true
opt.breakindent = true

-- Line Numbers & UI
opt.number = true
opt.signcolumn = 'yes'
opt.cursorline = true
opt.scrolloff = 10
opt.sidescrolloff = 8
opt.wrap = true
opt.linebreak = true -- wrap at word boundaries

-- Cursor
opt.guicursor = 'a:ver25'

-- Mouse & Clipboard
opt.mouse = "a"
opt.clipboard = 'unnamedplus'
opt.mousefocus = true

-- Files & Undo
opt.undofile = true
opt.undolevels = 10000
opt.backup = false
opt.writebackup = false
opt.swapfile = false

-- Search & Completion
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.inccommand = 'split'
opt.completeopt =
"menu,menuone,noselect,popup" -- Ensures the menu appears even for a single match and uses the native popup window.

-- Splits
opt.splitright = true
opt.splitbelow = true

-- Performance
opt.updatetime = 250
opt.timeoutlen = 300

-- Folding
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldenable = true

-- Diff
opt.diffopt = "internal,filler,closeoff,indent-heuristic,linematch:60,algorithm:histogram"

-- Misc
opt.confirm = true
opt.shortmess:append("sI") -- disable intro message

-- ============================================================================
-- Key Mappings
-- ============================================================================
local map = vim.keymap.set

-- Clear search highlight
map('n', '<Esc>', '<cmd>nohlsearch<CR>', { desc = 'Clear search highlight' })

-- Better window navigation
map('n', '<C-h>', '<C-w><C-h>', { desc = 'Focus left window' })
map('n', '<C-l>', '<C-w><C-l>', { desc = 'Focus right window' })
map('n', '<C-j>', '<C-w><C-j>', { desc = 'Focus lower window' })
map('n', '<C-k>', '<C-w><C-k>', { desc = 'Focus upper window' })

-- Better indenting
map('v', '<', '<gv', { desc = 'Indent left' })
map('v', '>', '>gv', { desc = 'Indent right' })

-- Move lines
map('n', '<A-j>', ':m .+1<CR>==', { desc = 'Move line down', silent = true })
map('n', '<A-k>', ':m .-2<CR>==', { desc = 'Move line up', silent = true })
map('v', '<A-j>', ":m '>+1<CR>gv=gv", { desc = 'Move selection down', silent = true })
map('v', '<A-k>', ":m '<-2<CR>gv=gv", { desc = 'Move selection up', silent = true })

-- Better paste (don't yank replaced text)
map('x', '<leader>p', [["_dP]], { desc = 'Paste without yank' })

-- Save & Quit shortcuts
map('n', '<leader>w', '<cmd>w<CR>', { desc = 'Save file' })
map('n', '<leader>q', '<cmd>q<CR>', { desc = 'Quit' })
map('n', '<leader>Q', '<cmd>qa!<CR>', { desc = 'Quit all force' })

-- Buffer navigation
map('n', '<S-h>', '<cmd>bprevious<CR>', { desc = 'Previous buffer' })
map('n', '<S-l>', '<cmd>bnext<CR>', { desc = 'Next buffer' })
map('n', '<leader>bd', '<cmd>bdelete<CR>', { desc = 'Delete buffer' })

-- ============================================================================
-- Autocommands
-- ============================================================================
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Highlight on yank
autocmd('TextYankPost', {
    desc = 'Highlight when yanking text',
    group = augroup('highlight-yank', { clear = true }),
    callback = function()
        vim.hl.on_yank({ timeout = 200 })
    end,
})

-- Auto-resize splits when window is resized
autocmd('VimResized', {
    group = augroup('resize-splits', { clear = true }),
    callback = function()
        vim.cmd('tabdo wincmd =')
    end,
})

-- Close some filetypes with <q>
autocmd('FileType', {
    group = augroup('close-with-q', { clear = true }),
    pattern = { 'help', 'lspinfo', 'man', 'qf', 'query', 'checkhealth' },
    callback = function(event)
        vim.bo[event.buf].buflisted = false
        map('n', 'q', '<cmd>close<CR>', { buffer = event.buf, silent = true })
    end,
})

-- Check if we need to reload file when it changed
autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
    group = augroup('checktime', { clear = true }),
    command = 'checktime',
})

-- ============================================================================
-- LSP Configuration (Neovim 0.12 native)
-- ============================================================================
-- Keymaps on LspAttach
vim.api.nvim_create_autocmd('LspAttach', {
    group = augroup('lsp-attach', { clear = true }),
    callback = function(event)
        local lsp_map = function(keys, func, desc, mode)
            mode = mode or 'n'
            vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
        end

        -- Navigation
        lsp_map('gd', vim.lsp.buf.definition, 'Goto Definition')
        lsp_map('gD', vim.lsp.buf.declaration, 'Goto Declaration')
        lsp_map('gr', vim.lsp.buf.references, 'Goto References')
        lsp_map('gi', vim.lsp.buf.implementation, 'Goto Implementation')
        lsp_map('gy', vim.lsp.buf.type_definition, 'Goto Type Definition')

        -- Code Actions
        lsp_map('<leader>ca', vim.lsp.buf.code_action, 'Code Action', { 'n', 'x' })
        lsp_map('<leader>rn', vim.lsp.buf.rename, 'Rename')

        -- Diagnostics
        lsp_map('<leader>e', vim.diagnostic.open_float, 'Show Diagnostic')
        lsp_map('<leader>dl', vim.diagnostic.setloclist, 'Diagnostic List')

        -- Hover
        lsp_map('K', vim.lsp.buf.hover, 'Hover Documentation')

        -- Document Highlight on CursorHold
        local client = vim.lsp.get_clients({ id = event.data.client_id })[1]
        if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
            local highlight_augroup = vim.api.nvim_create_augroup('lsp-highlight', { clear = false })
            vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
                buffer = event.buf,
                group = highlight_augroup,
                callback = vim.lsp.buf.document_highlight,
            })
            vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
                buffer = event.buf,
                group = highlight_augroup,
                callback = vim.lsp.buf.clear_references,
            })
            vim.api.nvim_create_autocmd('LspDetach', {
                group = vim.api.nvim_create_augroup('lsp-detach', { clear = true }),
                callback = function(event2)
                    vim.lsp.buf.clear_references()
                    vim.api.nvim_clear_autocmds { group = 'lsp-highlight', buffer = event2.buf }
                end,
            })
        end

        -- Inlay Hints Toggle
        if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
            lsp_map('<leader>th', function()
                vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
            end, 'Toggle Inlay Hints')
        end
    end,
})

-- Diagnostic Configuration
vim.diagnostic.config {
    severity_sort = true,
    float = {
        border = 'rounded',
        source = true,
        header = '',
        prefix = '',
    },
    underline = true,
    update_in_insert = false,
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = '󰅚',
            [vim.diagnostic.severity.WARN] = '󰀪',
            [vim.diagnostic.severity.INFO] = '󰋽',
            [vim.diagnostic.severity.HINT] = '󰌶',
        },
    },
    virtual_text = {
        spacing = 4,
        source = true,
        prefix = '●',
    },
}

-- ============================================================================
-- Plugin Manager: vim.pack (built-in)
-- ============================================================================
local gh = function(x) return 'https://github.com/' .. x end

vim.pack.add({
    -- Dependencies & Libraries
    gh('nvim-tree/nvim-web-devicons'),
    gh('nvim-lua/plenary.nvim'),

    -- Git Integration
    gh('lewis6991/gitsigns.nvim'),
    gh('NeogitOrg/neogit'),
    gh('sindrets/diffview.nvim'),

    -- Editor Enhancements
    gh('echasnovski/mini.surround'),
    gh('folke/todo-comments.nvim'),

    -- File Explorer & Navigation
    gh('nvim-tree/nvim-tree.lua'),
    gh('ibhagwan/fzf-lua'),

    -- UI & Appearance
    gh('folke/tokyonight.nvim'),
    gh('lukas-reineke/indent-blankline.nvim'),
    gh('romus204/tree-sitter-manager.nvim'),
    gh('nvim-treesitter/nvim-treesitter-context'),
    gh('nvim-treesitter/nvim-treesitter-textobjects'),
    gh('HiPhish/rainbow-delimiters.nvim'),

    -- LSP & Completion
    gh('folke/lazydev.nvim'),
    gh('rafamadriz/friendly-snippets'),
    { src = gh('saghen/blink.cmp'), version = vim.version.range('^1') },
    gh('neovim/nvim-lspconfig'),
    gh('windwp/nvim-autopairs'),
    gh('stevearc/conform.nvim'),

    -- Utilities
    gh('folke/which-key.nvim'),
    gh('nvim-lualine/lualine.nvim'),

    -- Language-Specific
    gh('spacewander/openresty-vim'),
    gh('MeanderingProgrammer/render-markdown.nvim'),
    gh('mistweaverco/kulala.nvim'),

    -- Terminal
    gh('akinsho/toggleterm.nvim'),
})

-- ============================================================================
-- Plugin Configuration
-- ============================================================================

-- Colorscheme (configure early)
require("tokyonight").setup({
    style = "night",
    on_highlights = function(hl, c)
        hl.CursorLineNr = { fg = c.orange, bold = true }
    end,
})
vim.cmd.colorscheme('tokyonight-night')

-- Icons
require('nvim-web-devicons').setup()

-- Git Signs
require('gitsigns').setup({
    numhl = true,
    signs = {
        add = { text = "+" },
    },
    on_attach = function(bufnr)
        local gs = require('gitsigns')
        local function gsmap(mode, l, r, desc)
            vim.keymap.set(mode, l, r, { buffer = bufnr, desc = 'Git: ' .. desc })
        end
        gsmap('n', ']h', function() gs.nav_hunk('next') end, 'Next hunk')
        gsmap('n', '[h', function() gs.nav_hunk('prev') end, 'Prev hunk')
        gsmap('n', '<leader>hs', gs.stage_hunk, 'Stage hunk')
        gsmap('v', '<leader>hs', function() gs.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end,
            'Stage hunk')
        gsmap('n', '<leader>hr', gs.reset_hunk, 'Reset hunk')
        gsmap('v', '<leader>hr', function() gs.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end,
            'Reset hunk')
        gsmap('n', '<leader>hp', gs.preview_hunk, 'Preview hunk')
        gsmap('n', '<leader>hb', function() gs.blame_line({ full = true }) end, 'Blame line')
        gsmap('n', '<leader>hd', gs.diffthis, 'Diff this')
    end,
})

-- Neogit & Diffview
require('neogit').setup({ integrations = { diffview = true } })
require('diffview').setup()
map('n', '<leader>gg', '<cmd>Neogit<cr>', { desc = 'Show Neogit UI' })
map('n', '<leader>dv', '<cmd>DiffviewOpen<cr>', { desc = 'DiffView Open' })
map('n', '<leader>dc', '<cmd>DiffviewClose<cr>', { desc = 'DiffView Close' })

-- Mini Surround
require('mini.surround').setup()

-- Todo Comments
require('todo-comments').setup()

-- NvimTree
require('nvim-tree').setup()
map('n', '<leader>nt', '<cmd>NvimTreeToggle<CR>', { desc = 'NvimTree' })

-- FzfLua
map('n', '<leader>f/', '<cmd>FzfLua <CR>', { desc = 'FzfLua self' })
map('n', '<leader>ff', '<cmd>FzfLua files<CR>', { desc = 'files' })
map('n', '<leader>fb', '<cmd>FzfLua buffers<CR>', { desc = 'buffers' })
map('n', '<leader>fl', '<cmd>FzfLua live_grep<CR>', { desc = 'live grep' })
map('n', '<leader>fh', '<cmd>FzfLua help_tags<CR>', { desc = 'help' })
map('n', '<leader>fH', '<cmd>FzfLua highlights<CR>', { desc = 'highlights' })
map('n', '<leader>fm', '<cmd>FzfLua oldfiles<CR>', { desc = 'mru' })
map('n', '<leader>fc', '<cmd>FzfLua commands<CR>', { desc = 'commands' })
map('n', '<leader>fj', '<cmd>FzfLua jumps<CR>', { desc = 'jumplist' })
map('n', '<leader>fk', '<cmd>FzfLua keymaps<CR>', { desc = 'keymaps' })
map('n', '<leader>fq', '<cmd>FzfLua quickfix<CR>', { desc = 'quickfix' })
map('n', '<leader>fw', '<cmd>FzfLua grep_cword<CR>', { desc = 'cword' })
map('n', '<leader>fD', '<cmd>FzfLua diagnostics_document<CR>', { desc = 'document diagnostics' })
map('n', '<leader>fd', '<cmd>FzfLua lsp_definitions<CR>', { desc = 'lsp_definition' })
map('n', '<leader>fr', '<cmd>FzfLua lsp_references<CR>', { desc = 'lsp_references' })
map('n', '<leader>fi', '<cmd>FzfLua lsp_implementations<CR>', { desc = 'lsp_implementations' })
map('n', '<leader>fs', '<cmd>FzfLua lsp_document_symbols<CR>', { desc = 'lsp_document_symbols' })
map('n', '<leader>fS', '<cmd>FzfLua lsp_workspace_symbols<CR>', { desc = 'lsp_workspace_symbols' })

-- Indent Blankline
require('ibl').setup()

-- Tree-sitter Manager (parser installer, replaces nvim-treesitter)
require('tree-sitter-manager').setup({
    ensure_installed = {
        "bash", "c", "cmake", "cpp", "css", "dockerfile",
        "dot", "doxygen", "diff", "git_config", "gitignore",
        "go", "gomod", "gosum", "gowork",
        "html", "http", "javascript", "json", "lua",
        "make", "markdown", "markdown_inline",
        "proto", "python", "query", "regex",
        "sql", "toml", "typescript", "vim", "vimdoc", "yaml",
    },
    auto_install = true,
})

-- Treesitter folding (highlighting handled by tree-sitter-manager)
vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('ts-features', { clear = true }),
    pattern = "*",
    callback = function()
        if vim.treesitter.get_parser(0, nil, { error = false }) then
            vim.wo[0][0].foldmethod = 'expr'
            vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
        end
    end,
})

-- Treesitter Context
require('treesitter-context').setup({ max_lines = 3 })

-- Treesitter Textobjects
local tst = require("nvim-treesitter-textobjects")
tst.setup {
    select = { lookahead = true },
    move = { set_jumps = true },
}

local select_to = require("nvim-treesitter-textobjects.select")
local swap = require("nvim-treesitter-textobjects.swap")
local move = require("nvim-treesitter-textobjects.move")

for _, mode in ipairs({ "x", "o" }) do
    vim.keymap.set(mode, "af", function() select_to.select_textobject("@function.outer", "textobjects") end,
        { desc = "outer function" })
    vim.keymap.set(mode, "if", function() select_to.select_textobject("@function.inner", "textobjects") end,
        { desc = "inner function" })
    vim.keymap.set(mode, "ac", function() select_to.select_textobject("@class.outer", "textobjects") end,
        { desc = "outer class" })
    vim.keymap.set(mode, "ic", function() select_to.select_textobject("@class.inner", "textobjects") end,
        { desc = "inner class" })
    vim.keymap.set(mode, "aa", function() select_to.select_textobject("@parameter.outer", "textobjects") end,
        { desc = "outer parameter" })
    vim.keymap.set(mode, "ia", function() select_to.select_textobject("@parameter.inner", "textobjects") end,
        { desc = "inner parameter" })
    vim.keymap.set(mode, "ab", function() select_to.select_textobject("@block.outer", "textobjects") end,
        { desc = "outer block" })
    vim.keymap.set(mode, "ib", function() select_to.select_textobject("@block.inner", "textobjects") end,
        { desc = "inner block" })
end

vim.keymap.set("n", "<leader>sn", function() swap.swap_next("@parameter.inner") end,
    { desc = "Swap next parameter" })
vim.keymap.set("n", "<leader>sp", function() swap.swap_previous("@parameter.inner") end,
    { desc = "Swap prev parameter" })

for _, mode in ipairs({ "n", "x", "o" }) do
    vim.keymap.set(mode, "]f", function() move.goto_next_start("@function.outer", "textobjects") end,
        { desc = "Next function start" })
    vim.keymap.set(mode, "]c", function() move.goto_next_start("@class.outer", "textobjects") end,
        { desc = "Next class start" })
    vim.keymap.set(mode, "]F", function() move.goto_next_end("@function.outer", "textobjects") end,
        { desc = "Next function end" })
    vim.keymap.set(mode, "]C", function() move.goto_next_end("@class.outer", "textobjects") end,
        { desc = "Next class end" })
    vim.keymap.set(mode, "[f", function() move.goto_previous_start("@function.outer", "textobjects") end,
        { desc = "Prev function start" })
    vim.keymap.set(mode, "[c", function() move.goto_previous_start("@class.outer", "textobjects") end,
        { desc = "Prev class start" })
    vim.keymap.set(mode, "[F", function() move.goto_previous_end("@function.outer", "textobjects") end,
        { desc = "Prev function end" })
    vim.keymap.set(mode, "[C", function() move.goto_previous_end("@class.outer", "textobjects") end,
        { desc = "Prev class end" })
end

-- Rainbow Delimiters
local rainbow = require('rainbow-delimiters')
vim.g.rainbow_delimiters = {
    strategy = {
        [''] = rainbow.strategy['global'],
        vim = rainbow.strategy['local'],
    },
    query = {
        [''] = 'rainbow-delimiters',
        lua = 'rainbow-blocks',
    },
}

-- Lazydev (Lua LSP helper)
require('lazydev').setup({
    library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
    },
})

-- Blink.cmp (Completion)
local blink = require('blink.cmp')
blink.setup({
    appearance = {
        nerd_font_variant = 'mono',
    },

    sources = {
        default = { "lsp", "path", "snippets", "buffer" },
        per_filetype = {
            lua = { inherit_defaults = true, "lazydev" },
        },
        providers = {
            lazydev = {
                name = "LazyDev",
                module = "lazydev.integrations.blink",
                score_offset = 100,
                fallbacks = { "lsp" },
            },
            buffer = {
                max_items = 4,
                min_keyword_length = 4,
            },
            snippets = {
                should_show_items = function(ctx)
                    return ctx.trigger.initial_kind ~= 'trigger_character'
                end,
            },
        },
    },

    keymap = {
        preset = 'enter',
        ['<Tab>'] = { 'snippet_forward', 'select_next', 'fallback' },
        ['<S-Tab>'] = { 'snippet_backward', 'select_prev', 'fallback' },
        ['<C-b>'] = { 'scroll_documentation_up', 'fallback' },
        ['<C-f>'] = { 'scroll_documentation_down', 'fallback' },
    },

    completion = {
        keyword = { range = 'full' },
        accept = { auto_brackets = { enabled = true } },
        list = { selection = { preselect = false, auto_insert = true } },
        menu = {
            draw = {
                treesitter = { 'lsp' },
                columns = {
                    { "label",     "label_description", gap = 1 },
                    { "kind_icon", "kind" }
                },
            },
        },
        documentation = { auto_show = true, auto_show_delay_ms = 500 },
        ghost_text = { enabled = true },
    },

    signature = { enabled = true },

    cmdline = { enabled = true },
})

-- Set blink.cmp capabilities for all LSP servers
vim.lsp.config('*', {
    capabilities = blink.get_lsp_capabilities(),
})

-- Autopairs
require("nvim-autopairs").setup({
    check_ts = true,
    ts_config = {
        lua = { 'string' },
        javascript = { 'template_string' },
        java = false,
    },
    disable_filetype = { "FzfLua", "vim" },
    fast_wrap = {
        map = '<M-e>',
        chars = { '{', '[', '(', '"', "'" },
        pattern = [=[[%'%"%>%]%)%}%,]]=],
        end_key = '$',
        keys = 'qwertyuiopzxcvbnmasdfghjkl',
        check_comma = true,
        highlight = 'Search',
        highlight_grey = 'Comment'
    },
})

-- Conform (Formatter)
vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
require("conform").setup({
    default_format_opts = {
        lsp_format = "fallback",
    },

    formatters_by_ft = {
        lua = { lsp_format = "prefer" },
        sh = { "shfmt" },
        bash = { "shfmt" },
        sql = { "sqlfluff" },
        json = { "jq" },
        yaml = { "prettierd", "prettier", stop_after_first = true },
        markdown = { "prettierd", "prettier", stop_after_first = true },
        proto = { "clang_format" },
        c = { "clang_format" },
        cpp = { "clang_format" },
        go = { "goimports", "gofumpt" },
        python = { "ruff_format" },
        javascript = { "prettierd", "prettier", stop_after_first = true },
        typescript = { "prettierd", "prettier", stop_after_first = true },
        ["_"] = { "trim_whitespace", "trim_newlines" },
    },

    format_on_save = {
        lsp_format = "fallback",
        timeout_ms = 500,
    },

    formatters = {
        shfmt = {
            append_args = { "-i", "4", "-ci", "-bn", "-sr" },
        },
        prettier = {
            append_args = { "--tab-width", "4" },
        },
        clang_format = {
            append_args = { "--style=file" },
        },
    },
})
map({ 'n', 'v' }, '<leader>bf', function()
    require("conform").format({ async = true })
end, { desc = 'Format Buffer' })

-- Which-key
require('which-key').setup({
    preset = "modern",
    spec = {
        { "<leader>b", group = "Buffer" },
        { "<leader>c", group = "Code" },
        { "<leader>d", group = "Diagnostic/Diff" },
        { "<leader>f", group = "Find" },
        { "<leader>h", group = "Git Hunk" },
        { "<leader>n", group = "NvimTree/Neogit" },
        { "<leader>r", group = "Rename/HTTP" },
        { "<leader>s", group = "Swap" },
        { "<leader>t", group = "Toggle" },
        { "<leader>p", group = "Peek/Paste" },
        { "[",         group = "Previous" },
        { "]",         group = "Next" },
        { "g",         group = "Goto" },
    },
})

-- Lualine
require('lualine').setup({
    extensions = { "toggleterm", "nvim-tree", "fzf" },
})

-- Render Markdown
require('render-markdown').setup({
    completions = {
        blink = { enabled = true },
    },
})

-- Kulala (HTTP Client)
require('kulala').setup()

-- Toggleterm
require("toggleterm").setup({
    open_mapping = [[<c-\>]],
    shading_factor = 2,
    direction = "float",
    float_opts = {
        border = "curved",
        width = function() return math.floor(vim.o.columns * 0.9) end,
        height = function() return math.floor(vim.o.lines * 0.85) end,
    },
})

-- ============================================================================
-- LSP Server Configurations (after vim.pack.add so nvim-lspconfig defaults are loaded)
-- ============================================================================
vim.lsp.config('clangd', {
    cmd = {
        'clangd',
        '--background-index',
        '--clang-tidy',
        '--completion-style=detailed',
        '--header-insertion=iwyu',
        '--pch-storage=disk',
        '-j=4',
    },
})

vim.lsp.config('lua_ls', {
    settings = {
        Lua = {
            runtime = { version = 'LuaJIT' },
            completion = {
                callSnippet = 'Replace',
                displayContext = 1,
            },
            diagnostics = {
                disable = { 'missing-fields' },
            },
            workspace = {
                checkThirdParty = false,
            },
            telemetry = { enable = false },
            format = {
                enable = true,
                defaultConfig = {
                    indent_style = "space",
                    indent_size = "4",
                }
            },
            hint = {
                enable = true,
                setType = true,
            },
        },
    },
})

vim.lsp.config('gopls', {
    settings = {
        gopls = {
            analyses = {
                unusedparams = true,
                shadow = true,
            },
            staticcheck = true,
            gofumpt = true,
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
        },
    },
})

vim.lsp.enable({ 'bashls', 'clangd', 'lua_ls', 'gopls', 'neocmake' })
