-- ============================================================================
-- Leader Keys (must be set before lazy.nvim)
-- ============================================================================
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

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
opt.showmatch = true
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
opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- Splits
opt.splitright = true
opt.splitbelow = true

-- Performance
opt.updatetime = 250
opt.timeoutlen = 300
opt.lazyredraw = false -- treesitter conflicts

-- Folding (treesitter-based)
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
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
map('n', '<A-j>', ':m .+1<CR>==', { desc = 'Move line down' })
map('n', '<A-k>', ':m .-2<CR>==', { desc = 'Move line up' })
map('v', '<A-j>', ":m '>+1<CR>gv=gv", { desc = 'Move selection down' })
map('v', '<A-k>', ":m '<-2<CR>gv=gv", { desc = 'Move selection up' })

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

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
    vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", lazypath })
end
vim.opt.rtp:prepend(lazypath)

-- ============================================================================
-- Plugin Manager & Plugin Specifications
-- ============================================================================
require("lazy").setup({
    -- ========================================================================
    -- Dependencies & Libraries
    -- ========================================================================
    { "nvim-tree/nvim-web-devicons", lazy = true },

    -- ========================================================================
    -- Git Integration
    -- ========================================================================
    {
        "lewis6991/gitsigns.nvim",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require('gitsigns').setup({
                numhl = true,
                signs = {
                    add = { text = "+" },
                },
            })
        end
    },

    {
        "sindrets/diffview.nvim",
        lazy = true,
        keys = {
            { "<leader>dv", "<cmd>DiffviewOpen<cr>",  desc = "DiffView Open" },
            { "<leader>dc", "<cmd>DiffviewClose<cr>", desc = "DiffView Close" },
        },
    },

    {
        "NeogitOrg/neogit",
        lazy = true,
        keys = {
            { "<leader>gg", "<cmd>Neogit<cr>", desc = "Show Neogit UI" }
        },
        config = function()
            require('neogit').setup({
                integrations = {
                    diffview = true,
                },
            })
        end
    },

    -- ========================================================================
    -- Editor Enhancements
    -- ========================================================================
    { 'echasnovski/mini.surround', event = "VeryLazy", opts = {} },

    {
        'folke/todo-comments.nvim',
        event = { "BufReadPost", "BufNewFile" },
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {},
    },

    -- ========================================================================
    -- File Explorer & Navigation
    -- ========================================================================
    {
        "nvim-tree/nvim-tree.lua",
        keys = { { "<leader>nt", "<cmd>NvimTreeToggle<CR>", desc = "NvimTree" } },
        config = function()
            require("nvim-tree").setup {}
        end,
    },

    -- FZF: Fuzzy Finder
    {
        "ibhagwan/fzf-lua",
        cmd = "FzfLua",
        keys = {
            { "<leader>f/", "<cmd>FzfLua <CR>",                                      desc = "FzfLua self" },
            { "<leader>ff", "<cmd>FzfLua files<CR>",                                 desc = "files" },
            { "<leader>fb", "<cmd>FzfLua buffers<CR>",                               desc = "buffers" },
            { "<leader>fl", "<cmd>FzfLua live_grep<CR>",                             desc = "live grep" },
            { "<leader>fh", "<cmd>FzfLua help_tags<CR>",                             desc = "help" },
            { "<leader>fH", "<cmd>FzfLua highlights<CR>",                            desc = "highlights" },
            { "<leader>fm", "<cmd>FzfLua oldfiles<CR>",                              desc = "mru" }, -- mru: most recent used
            { "<leader>fc", "<cmd>FzfLua commands<CR>",                              desc = "commands" },
            { "<leader>fj", "<cmd>FzfLua jumps<CR>",                                 desc = "jumplist" },
            { "<leader>fk", "<cmd>FzfLua keymaps<CR>",                               desc = "keymaps" },
            { "<leader>fq", "<cmd>FzfLua quickfix<CR>",                              desc = "quickfix" },
            { "<leader>fw", "<cmd>FzfLua grep_cword<CR>",                            desc = "cword" },
            { "<leader>fa", "<cmd>lua require('helper.asynctask').fzf_select()<CR>", desc = "asynctask" },
            { "<leader>fD", "<cmd>FzfLua lsp_document_diagnostics<CR>",              desc = "lsp_document_diagnostics" },
            { "<leader>fd", "<cmd>FzfLua lsp_definitions<CR>",                       desc = "lsp_definition" },
            { "<leader>fr", "<cmd>FzfLua lsp_references<CR>",                        desc = "lsp_references" },
            { "<leader>fi", "<cmd>FzfLua lsp_implementations<CR>",                   desc = "lsp_implementations" },
            { "<leader>fs", "<cmd>FzfLua lsp_document_symbols<CR>",                  desc = "lsp_document_symbols" },
            { "<leader>fS", "<cmd>FzfLua lsp_workspace_symbols<CR>",                 desc = "lsp_workspace_symbols" },
            { "<C-f>",      "<cmd>FzfLua grep_curbuf<CR>",                           desc = "lines" },
        },
    },

    -- ========================================================================
    -- UI & Appearance
    -- ========================================================================
    -- Colorscheme
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                style = "night",
                on_highlights = function(hl, c)
                    hl.CursorLineNr = { fg = c.orange, bold = true }
                end,
            })
            vim.cmd.colorscheme('tokyonight-night')
        end,
    },

    -- Indent Guides
    {
        "lukas-reineke/indent-blankline.nvim",
        main = "ibl",
        event = { "BufReadPost", "BufNewFile" },
        config = function()
            require("ibl").setup()
        end
    },

    -- Treesitter: Syntax Highlighting & Text Objects
    {
        "nvim-treesitter/nvim-treesitter",
        lazy = false,
        build = ":TSUpdate",
        dependencies = {
            { "nvim-treesitter/nvim-treesitter-context", opts = { max_lines = 3 } },
        },
        config = function()
            local ts = require('nvim-treesitter')

            ts.setup {}

            -- Install parsers (runs async)
            ts.install {
                "bash", "cmake", "comment", "cpp", "css", "dockerfile",
                "dot", "doxygen", "diff", "git_config", "gitignore",
                "go", "gomod", "gosum", "gowork",
                "html", "http", "javascript", "json", "lua",
                "make", "proto", "python", "regex", "sql", "toml", "typescript", "yaml",
            }

            -- Treesitter-based indentation (replaces indent.enable)
            vim.api.nvim_create_autocmd('FileType', {
                group = vim.api.nvim_create_augroup('ts-indent', { clear = true }),
                callback = function()
                    local disabled = { python = true, yaml = true }
                    if not disabled[vim.bo.filetype] then
                        vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
                    end
                end,
            })
        end,
    },

    -- Treesitter Textobjects
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        event = { "BufReadPost", "BufNewFile" },
        config = function()
            local tst = require("nvim-treesitter-textobjects")
            tst.setup {
                select = { lookahead = true },
                move = { set_jumps = true },
            }

            local select_to = require("nvim-treesitter-textobjects.select")
            local swap = require("nvim-treesitter-textobjects.swap")
            local move = require("nvim-treesitter-textobjects.move")

            -- Select
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

            -- Swap
            vim.keymap.set("n", "<leader>a", function() swap.swap_next("@parameter.inner") end,
                { desc = "Swap next parameter" })
            vim.keymap.set("n", "<leader>A", function() swap.swap_previous("@parameter.inner") end,
                { desc = "Swap prev parameter" })

            -- Move
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
        end,
    },

    -- Rainbow Delimiters
    {
        "HiPhish/rainbow-delimiters.nvim",
        event = { "BufReadPost", "BufNewFile" },
        config = function()
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
        end,
    },

    -- ========================================================================
    -- LSP & Completion
    -- ========================================================================
    {
        "folke/lazydev.nvim",
        ft = "lua", -- only load on lua files
        opts = {
            library = {
                -- See the configuration section for more details
                -- Load luvit types when the `vim.uv` word is found
                { path = "${3rd}/luv/library", words = { "vim%.uv" } },
            },
        },
    },

    -- Blink.cmp: Completion Engine
    {
        'saghen/blink.cmp',
        event = "InsertEnter",
        version = '1.*',
        opts_extend = { "sources.default" },
        config = function()
            local blink = require('blink.cmp')
            blink.setup({
                sources = {
                    default = { "lazydev", "lsp", "path", "snippets", "buffer" },
                    per_filetype = {
                        lua = { "lazydev", "lsp", "path", "snippets", "buffer" },
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
                    },
                },

                keymap = {
                    preset = 'default',
                    ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
                    ['<C-e>'] = { 'hide', 'fallback' },
                    ['<CR>'] = { 'accept', 'fallback' },
                    ['<Tab>'] = { 'select_next', 'snippet_forward', 'fallback' },
                    ['<S-Tab>'] = { 'select_prev', 'snippet_backward', 'fallback' },
                    ['<C-n>'] = { 'select_next', 'fallback' },
                    ['<C-p>'] = { 'select_prev', 'fallback' },
                    ['<C-u>'] = { 'scroll_documentation_up', 'fallback' },
                    ['<C-d>'] = { 'scroll_documentation_down', 'fallback' },
                },

                completion = {
                    keyword = { range = 'full' },
                    accept = { auto_brackets = { enabled = true } },
                    list = { selection = { preselect = false, auto_insert = true } },
                    menu = {
                        draw = {
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
        end,
    },

    -- nvim-lspconfig: provides default LSP server configs (lsp/*.lua) for vim.lsp.config
    { "neovim/nvim-lspconfig",     lazy = false },

    -- Autopairs
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        config = function()
            require("nvim-autopairs").setup({
                check_ts = true,
                ts_config = {
                    lua = { 'string' },
                    javascript = { 'template_string' },
                    java = false,
                },
                disable_filetype = { "TelescopePrompt", "vim" },
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
        end
    },

    -- Conform: Code Formatter
    {
        "stevearc/conform.nvim",
        event = { "BufWritePre" },
        cmd = { "ConformInfo" },
        keys = {
            {
                "<leader>bf",
                function()
                    require("conform").format({ async = true, lsp_format = "fallback" })
                end,
                mode = { "n", "v" },
                desc = "Format Buffer",
            },
        },

        init = function()
            vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
        end,

        config = function()
            require("conform").setup({
                formatters_by_ft = {
                    lua = { lsp_format = "prefer" },
                    sh = { "shfmt" },
                    bash = { "shfmt" },
                    sql = { "sqlfluff" },
                    json = { "jq" },
                    yaml = { "prettier" },
                    markdown = { "prettier" },
                    proto = { "clang_format" },
                    c = { "clang_format" },
                    cpp = { "clang_format" },
                    go = { "gofumpt", "goimports" },
                    python = { "isort", "black" },
                    javascript = { "prettier" },
                    typescript = { "prettier" },
                    ["_"] = { "trim_whitespace" },
                },

                format_on_save = function(bufnr)
                    -- Disable for specific filetypes
                    local ignore_filetypes = { "sql", "java" }
                    if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
                        return
                    end
                    return { timeout_ms = 1000, lsp_format = "fallback" }
                end,

                formatters = {
                    shfmt = {
                        prepend_args = { "-i", "4", "-ci", "-bn", "-sr" },
                    },
                    prettier = {
                        prepend_args = { "--tab-width", "4" },
                    },
                    clang_format = {
                        prepend_args = { "--style=file" },
                    },
                },
            })
        end
    },

    -- ========================================================================
    -- Utilities
    -- ========================================================================
    -- Which-key: Show keybindings
    {
        "folke/which-key.nvim",
        event = 'VeryLazy',
        opts = {
            preset = "modern",
            spec = {
                { "<leader>b", group = "Buffer" },
                { "<leader>c", group = "Code" },
                { "<leader>d", group = "Diagnostic/Diff" },
                { "<leader>f", group = "Find" },
                { "<leader>n", group = "NvimTree/Neogit" },
                { "<leader>r", group = "Rename/HTTP" },
                { "<leader>t", group = "Toggle" },
                { "<leader>p", group = "Peek/Paste" },
                { "[",         group = "Previous" },
                { "]",         group = "Next" },
                { "g",         group = "Goto" },
            },
        },
    },

    -- Statusline
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        config = function()
            require('lualine').setup({
                extensions = { "lazy", "toggleterm", "nvim-tree", "fzf" },
            })
        end
    },

    -- ========================================================================
    -- Language-Specific Plugins
    -- ========================================================================
    { "spacewander/openresty-vim", ft = "nginx" },

    {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = "markdown",
        config = function()
            require('render-markdown').setup({
                completions = {
                    blink = { enabled = true },
                },
            })
        end,
    },

    -- HTTP Client
    {
        'mistweaverco/kulala.nvim',
        ft = { "http", "rest" },
        keys = {
            { "<leader>Rs", desc = "Send request" },
            { "<leader>Ra", desc = "Send all requests" },
            { "<leader>Rb", desc = "Open scratchpad" },
        },
    },

    -- ========================================================================
    -- Terminal
    -- ========================================================================
    {
        "akinsho/toggleterm.nvim",
        keys = {
            { [[<c-\>]], "<cmd>ToggleTerm<cr>", desc = "Toggle Terminal" },
        },
        config = function()
            require("toggleterm").setup({
                shading_factor = 2,
                direction = "float",
                float_opts = {
                    border = "curved",
                    width = function() return math.floor(vim.o.columns * 0.9) end,
                    height = function() return math.floor(vim.o.lines * 0.85) end,
                },
            })
        end
    },

    -- ============================================================================
    -- Lazy.nvim Config
    -- ============================================================================
}, {
    ui = {
        border = "rounded",
        icons = vim.g.have_nerd_font and {} or {
            cmd = "⌘",
            config = "🛠",
            event = "📅",
            ft = "📂",
            init = "⚙",
            keys = "🗝",
            plugin = "🔌",
            runtime = "💻",
            require = "🌙",
            source = "📄",
            start = "🚀",
            task = "📌",
        },
    },
    checker = { enabled = true, notify = false },
    performance = {
        rtp = {
            disabled_plugins = {
                "gzip",
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
            },
        },
    },
})

-- ============================================================================
-- LSP Server Configurations (after lazy.nvim so nvim-lspconfig defaults are loaded)
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
                globals = { 'vim' },
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

vim.lsp.config('bashls', {})
vim.lsp.config('neocmake', {})

vim.lsp.enable({ 'bashls', 'clangd', 'lua_ls', 'gopls', 'neocmake' })
