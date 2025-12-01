-- ============================================================================
-- Leader Keys (must be set before lazy.nvim)
-- ============================================================================
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

-- ============================================================================
-- Performance & Startup Optimization
-- ============================================================================
-- Faster loading times
vim.loader.enable()

-- Disable some builtin providers (use external tools instead)
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0

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
opt.termguicolors = true
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
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldenable = true

-- Diff
opt.diffopt = "internal,filler,closeoff,indent-heuristic,linematch:60,algorithm:histogram"

-- Misc
opt.confirm = true
opt.syntax = 'enable'
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
        vim.highlight.on_yank({ timeout = 200 })
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
    { "nvim-lua/plenary.nvim",       lazy = true },
    { "nvim-tree/nvim-web-devicons", lazy = true },

    -- ========================================================================
    -- Git Integration
    -- ========================================================================
    {
        "lewis6991/gitsigns.nvim",
        event = { "BufReadPre", "BufNewFile" },
        opts = {
            numhl = true,
            signs = {
                add = { text = "+" },
            },
        },
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
        opts = {
            integrations = {
                diffview = true,
            },
        },
        keys = {
            { "<leader>ng", "<Cmd>Neogit<CR>", desc = "Open Neogit" },
        },
    },

    -- ========================================================================
    -- Editor Enhancements
    -- ========================================================================
    { 'echasnovski/mini.surround', event = "VeryLazy", opts = {} },

    {
        'folke/todo-comments.nvim',
        event = { "BufReadPost", "BufNewFile" },
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            signs = true,
            keywords = {
                FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
                TODO = { icon = " ", color = "info" },
                HACK = { icon = " ", color = "warning" },
                WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
                PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
                NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
                TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
            },
        },
    },

    -- ========================================================================
    -- File Explorer & Navigation
    -- ========================================================================
    {
        "nvim-tree/nvim-tree.lua",
        event = 'CursorHold',
        keys = { { "<leader>nt", "<cmd>NvimTreeToggle<CR>", desc = "NvimTree" } },
        opts = {
            diagnostics = {
                enable = true,
            },
            modified = {
                enable = true,
            },
        },
    },

    -- FZF: Fuzzy Finder
    {
        "ibhagwan/fzf-lua",
        cmd = "FzfLua",
        dependencies = { "nvim-tree/nvim-web-devicons" },
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
        opts = {
            'default-title',
            winopts = {
                height = 0.85,
                width = 0.85,
                preview = {
                    layout = 'vertical',
                    vertical = 'down:50%',
                },
            },
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
        opts = {
            style = "night",
            transparent = false,
            terminal_colors = true,
            dim_inactive = false,
            on_highlights = function(hl, c)
                hl.CursorLineNr = { fg = c.orange, bold = true }
            end,
        },
        config = function(_, opts)
            require("tokyonight").setup(opts)
            vim.cmd.colorscheme('tokyonight-night')
        end,
    },

    -- Indent Guides
    {
        "lukas-reineke/indent-blankline.nvim",
        main = "ibl",
        event = { "BufReadPost", "BufNewFile" },
        opts = {
            indent = {
                char = "│",
                tab_char = "│",
            },
            scope = { enabled = false },
            exclude = {
                filetypes = {
                    "help", "alpha", "dashboard", "neo-tree", "Trouble",
                    "lazy", "mason", "notify", "toggleterm",
                },
            },
        },
    },

    -- Treesitter: Syntax Highlighting & Text Objects
    {
        "nvim-treesitter/nvim-treesitter",
        version = false,
        build = ":TSUpdate",
        event = { "BufReadPost", "BufNewFile" },
        cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
        dependencies = {
            { "nvim-treesitter/nvim-treesitter-context",    opts = { max_lines = 3 } },
            { "nvim-treesitter/nvim-treesitter-refactor" },
            { "nvim-treesitter/nvim-treesitter-textobjects" },
        },
        opts = {
            ensure_installed = {
                "bash", "c", "cmake", "comment", "cpp", "css", "dockerfile",
                "dot", "doxygen", "diff", "git_config", "gitignore",
                "go", "gomod", "gosum", "gowork",
                "html", "http", "javascript", "json", "jsonc",
                "vim", "vimdoc", "lua", "luadoc",
                "make", "markdown", "markdown_inline",
                "proto", "python", "regex", "sql", "toml", "typescript", "yaml",
            },
            auto_install = true,
            sync_install = false,

            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },

            indent = { enable = true, disable = { "python", "yaml" } },

            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "<CR>",
                    node_incremental = "<CR>",
                    scope_incremental = "<S-CR>",
                    node_decremental = "<BS>",
                },
            },

            refactor = {
                highlight_definitions = {
                    enable = true,
                    clear_on_cursor_move = true,
                },
                smart_rename = {
                    enable = true,
                    keymaps = { smart_rename = "grr" },
                },
                navigation = {
                    enable = true,
                    keymaps = {
                        goto_definition = "gnd",
                        list_definitions = "gnD",
                        list_definitions_toc = "gO",
                        goto_next_usage = "]u",
                        goto_previous_usage = "[u",
                    },
                },
            },

            textobjects = {
                select = {
                    enable = true,
                    lookahead = true,
                    keymaps = {
                        ["af"] = "@function.outer",
                        ["if"] = "@function.inner",
                        ["ac"] = "@class.outer",
                        ["ic"] = "@class.inner",
                        ["aa"] = "@parameter.outer",
                        ["ia"] = "@parameter.inner",
                        ["ab"] = "@block.outer",
                        ["ib"] = "@block.inner",
                    },
                },
                swap = {
                    enable = true,
                    swap_next = { ["<leader>a"] = "@parameter.inner" },
                    swap_previous = { ["<leader>A"] = "@parameter.inner" },
                },
                move = {
                    enable = true,
                    set_jumps = true,
                    goto_next_start = {
                        ["]f"] = "@function.outer",
                        ["]c"] = "@class.outer",
                    },
                    goto_next_end = {
                        ["]F"] = "@function.outer",
                        ["]C"] = "@class.outer",
                    },
                    goto_previous_start = {
                        ["[f"] = "@function.outer",
                        ["[c"] = "@class.outer",
                    },
                    goto_previous_end = {
                        ["[F"] = "@function.outer",
                        ["[C"] = "@class.outer",
                    },
                },
                lsp_interop = {
                    enable = true,
                    border = "rounded",
                    floating_preview_opts = {},
                    peek_definition_code = {
                        ["<leader>pf"] = "@function.outer",
                        ["<leader>pc"] = "@class.outer",
                    },
                },
            },
        },
        config = function(_, opts)
            require("nvim-treesitter.configs").setup(opts)
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
                { path = "luvit-meta/library", words = { "vim%.uv" } },
            },
        },
    },

    -- Blink.cmp: Completion Engine
    {
        'saghen/blink.cmp',
        event = "InsertEnter",
        version = '1.*',
        dependencies = {
            "folke/lazydev.nvim",
        },
        opts = {
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
        },
        opts_extend = { "sources.default" }
    },

    -- LSP Configuration
    {
        "neovim/nvim-lspconfig",
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            "saghen/blink.cmp",
        },
        config = function()
            -- ================================================================
            -- LSP Keymaps (on attach)
            -- ================================================================
            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('lsp-attach', { clear = true }),
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
                    lsp_map('gt', vim.lsp.buf.type_definition, 'Goto Type Definition')

                    -- Code Actions
                    lsp_map('<leader>ca', vim.lsp.buf.code_action, 'Code Action', { 'n', 'x' })
                    lsp_map('<leader>rn', vim.lsp.buf.rename, 'Rename')
                    lsp_map('<leader>bf', vim.lsp.buf.format, 'Format Buffer')

                    -- Diagnostics
                    lsp_map('<leader>e', vim.diagnostic.open_float, 'Show Diagnostic')
                    lsp_map('[d', vim.diagnostic.goto_prev, 'Previous Diagnostic')
                    lsp_map(']d', vim.diagnostic.goto_next, 'Next Diagnostic')
                    lsp_map('<leader>dl', vim.diagnostic.setloclist, 'Diagnostic List')

                    -- Hover & Signature
                    lsp_map('K', vim.lsp.buf.hover, 'Hover Documentation')
                    lsp_map('<C-k>', vim.lsp.buf.signature_help, 'Signature Help', 'i')

                    -- Workspace
                    lsp_map('<leader>wa', vim.lsp.buf.add_workspace_folder, 'Add Workspace Folder')
                    lsp_map('<leader>wr', vim.lsp.buf.remove_workspace_folder, 'Remove Workspace Folder')
                    lsp_map('<leader>wl', function()
                        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
                    end, 'List Workspace Folders')

                    -- Document Highlight on CursorHold
                    local client = vim.lsp.get_client_by_id(event.data.client_id)
                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
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
                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
                        lsp_map('<leader>th', function()
                            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
                        end, 'Toggle Inlay Hints')
                    end
                end,
            })

            -- ================================================================
            -- Diagnostic Configuration
            -- ================================================================
            vim.diagnostic.config {
                severity_sort = true,
                float = {
                    border = 'rounded',
                    source = 'if_many',
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
                    source = 'if_many',
                    prefix = '●',
                },
            }

            -- ================================================================
            -- Language Server Configurations
            -- ================================================================
            local capabilities = require("blink.cmp").get_lsp_capabilities()

            -- Clangd (C/C++)
            vim.lsp.config('clangd', {
                capabilities = capabilities,
                cmd = {
                    'clangd',
                    '--background-index',
                    '--clang-tidy',
                    '--completion-style=detailed',
                    '--header-insertion=iwyu',
                    '--pch-storage=disk',
                    '-j=4',
                },
                init_options = {
                    fallbackFlags = { vim.bo.filetype == 'cpp' and '-std=c++23' or '-std=c17' },
                },
            })

            -- Lua Language Server
            vim.lsp.config('lua_ls', {
                capabilities = capabilities,
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
                            library = vim.api.nvim_get_runtime_file("", true),
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

            -- Go Language Server
            vim.lsp.config('gopls', {
                capabilities = capabilities,
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

            -- Bash Language Server
            vim.lsp.config('bashls', {
                capabilities = capabilities,
            })

            -- CMake Language Server
            vim.lsp.config('neocmake', {
                capabilities = capabilities,
            })

            -- Enable all configured servers
            vim.lsp.enable({ "bashls", "clangd", "lua_ls", "gopls", "neocmake" })
        end,
    },

    -- Autopairs
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {
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
        },
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
                    require("conform").format({ async = true, lsp_fallback = true })
                end,
                mode = { "n", "v" },
                desc = "Format Buffer",
            },
        },
        opts = {
            formatters_by_ft = {
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
                return { timeout_ms = 1000, lsp_fallback = true }
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
        },
        init = function()
            vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
        end,
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
            icons = { mappings = vim.g.have_nerd_font },
            spec = {
                { "<leader>b", group = "Buffer" },
                { "<leader>c", group = "Code" },
                { "<leader>d", group = "Diagnostic/Diff" },
                { "<leader>f", group = "Find" },
                { "<leader>n", group = "NvimTree/Neogit" },
                { "<leader>r", group = "Rename/HTTP" },
                { "<leader>t", group = "Toggle" },
                { "<leader>w", group = "Workspace" },
                { "<leader>p", group = "Peek" },
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
        dependencies = { "nvim-tree/nvim-web-devicons" },
        opts = {
            sections = {
                lualine_a = { 'mode' },
                lualine_b = { 'branch', 'diff', 'diagnostics' },
                lualine_c = { { 'filename', path = 1 } },
                lualine_x = { 'encoding', 'fileformat', 'filetype' },
                lualine_y = { 'progress' },
                lualine_z = { 'location', 'searchcount' },
            },
            extensions = { "lazy", "toggleterm", "nvim-tree", "fzf" },
        },
    },

    -- ========================================================================
    -- Language-Specific Plugins
    -- ========================================================================
    { "spacewander/openresty-vim", ft = "nginx" },

    {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = "markdown",
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            heading = {
                enabled = true,
                icons = { '󰲡 ', '󰲣 ', '󰲥 ', '󰲧 ', '󰲩 ', '󰲫 ' },
            },
            code = {
                enabled = true,
                style = 'normal',
            },
        },
    },

    { "b0o/schemastore.nvim", lazy = true },

    -- HTTP Client
    {
        'mistweaverco/kulala.nvim',
        ft = "http",
        init = function()
            vim.filetype.add({
                extension = { ["http"] = "http" },
            })
        end,
        keys = {
            { "<leader>rr", "<cmd>lua require('kulala').run()<cr>",         desc = "HTTP: Send request" },
            { "<leader>rt", "<cmd>lua require('kulala').toggle_view()<cr>", desc = "HTTP: Toggle view" },
            { "<leader>rp", "<cmd>lua require('kulala').jump_prev()<cr>",   desc = "HTTP: Previous request" },
            { "<leader>rn", "<cmd>lua require('kulala').jump_next()<cr>",   desc = "HTTP: Next request" },
            { "<leader>rc", "<cmd>lua require('kulala').copy()<cr>",        desc = "HTTP: Copy as cURL" },
        },
        opts = {
            default_view = "body",
            show_icons = "on_request",
        },
    },

    -- ========================================================================
    -- Terminal
    -- ========================================================================
    {
        "akinsho/toggleterm.nvim",
        cmd = { "ToggleTerm", "TermExec" },
        keys = {
            { [[<c-\>]],    "<cmd>ToggleTerm<cr>",                      desc = "Toggle Terminal" },
            { "<leader>tf", "<cmd>ToggleTerm direction=float<cr>",      desc = "Terminal Float" },
            { "<leader>th", "<cmd>ToggleTerm direction=horizontal<cr>", desc = "Terminal Horizontal" },
            { "<leader>tv", "<cmd>ToggleTerm direction=vertical<cr>",   desc = "Terminal Vertical" },
        },
        opts = {
            open_mapping = [[<c-\>]],
            shade_terminals = true,
            shading_factor = 2,
            direction = "float",
            float_opts = {
                border = "curved",
                width = math.floor(vim.o.columns * 0.9),
                height = math.floor(vim.o.lines * 0.85),
            },
            size = function(term)
                if term.direction == "horizontal" then
                    return math.floor(vim.o.lines * 0.3)
                elseif term.direction == "vertical" then
                    return math.floor(vim.o.columns * 0.4)
                end
            end,
            on_open = function(term)
                vim.cmd("startinsert!")
                vim.api.nvim_buf_set_keymap(term.bufnr, "t", "<esc>", [[<C-\><C-n>]], { noremap = true, silent = true })
            end,
        },
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
