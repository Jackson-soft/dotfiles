-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
    vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", lazypath })
end
vim.opt.rtp:prepend(lazypath)

---- Plugins ----
require("lazy").setup({
    -- Some requied Lua plugins
    {
        "nvim-lua/plenary.nvim",
        lazy = true
    },

    -- icons
    {
        "nvim-tree/nvim-web-devicons",
        lazy = true
    },

    -- git
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

    { 'echasnovski/mini.surround', event = "VeryLazy", opts = {} },

    {
        'folke/todo-comments.nvim',
        event = 'VimEnter',
    },

    -- file tree
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

    -- UI to select things (files, grep results, open buffers...)
    {
        "ibhagwan/fzf-lua",
        event = { "VeryLazy" },
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
    -- Themes
    {
        "folke/tokyonight.nvim",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme('tokyonight-night')
        end,
        opts = {},
    },

    -- {
    --     "catppuccin/nvim",
    --     name = "catppuccin",
    --     priority = 1000,
    --     init = function()
    --         vim.cmd.colorscheme("catppuccin-macchiato")
    --     end,
    -- },

    -- Add indentation guides even on blank lines
    {
        "lukas-reineke/indent-blankline.nvim",
        main = "ibl",
        event = 'VeryLazy',
        opts = {},
    },

    -- Highlights
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        opts = {
            ensure_installed = {
                "bash",
                "c",
                "cmake",
                "comment",
                "cpp",
                "css",
                "dockerfile",
                "dot",
                "doxygen",
                "diff",
                "git_config",
                "gitignore",
                "go",
                "gomod",
                "gosum",
                "gowork",
                "html",
                "http",
                "javascript",
                "json",
                "jsonc",
                "vim",
                "vimdoc",
                "lua",
                "make",
                "markdown",
                "org",
                "proto",
                "python",
                "regex",
                "sql",
                "toml",
                "typescript",
                "yaml",
            },
            highlight = { enable = true },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "gnn",
                    node_incremental = "grn",
                    scope_incremental = "grc",
                    node_decremental = "grm",
                },
            },
            indent = { enable = true },
            refactor = {
                highlight_definitions = { enable = true, clear_on_cursor_move = true },
                smart_rename = {
                    enable = true,
                    keymaps = {
                        smart_rename = "grr",
                    },
                },
                navigation = {
                    enable = true,
                    keymaps = {
                        goto_definition = "gnd",
                        list_definitions = "gnD",
                        list_definitions_toc = "gO",
                        goto_next_usage = "<a-*>",
                        goto_previous_usage = "<a-#>",
                    },
                },
            },
            textobjects = {
                select = {
                    enable = true,
                    -- Automatically jump forward to textobj, similar to targets.vim
                    lookahead = true,
                    keymaps = {
                        -- You can use the capture groups defined in textobjects.scm
                        ["af"] = "@function.outer",
                        ["if"] = "@function.inner",
                        ["ac"] = "@class.outer",
                        ["ic"] = "@class.inner",
                    },
                },
                swap = {
                    enable = true,
                    swap_next = {
                        ["<leader>a"] = "@parameter.inner",
                    },
                    swap_previous = {
                        ["<leader>A"] = "@parameter.inner",
                    },
                },
                move = {
                    enable = true,
                    set_jumps = true, -- whether to set jumps in the jumplist
                    goto_next_start = {
                        ["]m"] = "@function.outer",
                        ["]]"] = "@class.outer",
                    },
                    goto_next_end = {
                        ["]M"] = "@function.outer",
                        ["]["] = "@class.outer",
                    },
                    goto_previous_start = {
                        ["[m"] = "@function.outer",
                        ["[["] = "@class.outer",
                    },
                    goto_previous_end = {
                        ["[M"] = "@function.outer",
                        ["[]"] = "@class.outer",
                    },
                },
                lsp_interop = {
                    enable = true,
                    border = "none",
                    peek_definition_code = {
                        ["df"] = "@function.outer",
                        ["dF"] = "@class.outer",
                    },
                },
            },
        },
        dependencies = {
            { "nvim-treesitter/nvim-treesitter-context" },
            { "nvim-treesitter/nvim-treesitter-refactor" },
            { "nvim-treesitter/nvim-treesitter-textobjects" },
        },
    },

    {
        "HiPhish/rainbow-delimiters.nvim",
        event = "BufReadPost",
    },

    {
        'nvimdev/lspsaga.nvim',
        event = 'LspAttach',
        config = function()
            require('lspsaga').setup({})
        end,
    },

    -- Completion and linting
    {
        "hrsh7th/nvim-cmp",
        event = "VimEnter",
        config = function()
            local cmp = require("cmp")

            cmp.setup({
                mapping = cmp.mapping.preset.insert({
                    ['<C-n>'] = cmp.mapping.select_next_item(),
                    ['<C-p>'] = cmp.mapping.select_prev_item(),
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true
                    }),

                    ["<Tab>"] = cmp.mapping(function()
                        if cmp.visible() then
                            cmp.select_next_item()
                        else
                            cmp.complete()
                        end
                    end, { "i", "s" }),

                    ["<S-Tab>"] = cmp.mapping(function()
                        if cmp.visible() then
                            cmp.select_prev_item()
                        else
                            cmp.complete()
                        end
                    end, { "i", "s" }),
                }),
                sources = cmp.config.sources({
                    { name = "lazydev",                group_index = 0, }, -- set group index to 0 to skip loading LuaLS completions
                    { name = "nvim_lsp" },
                    { name = "nvim_lua" },
                    { name = "buffer" },
                    { name = "path" },
                    { name = "treesitter" },
                    { name = 'nvim_lsp_signature_help' },
                }),
            })

            -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
            cmp.setup.cmdline({ '/', '?' }, {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'buffer' },
                })
            })

            -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
            cmp.setup.cmdline(':', {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'path' },
                    { name = 'cmdline' },
                }),
                matching = { disallow_symbol_nonprefix_matching = false }
            })
        end,
        dependencies = {
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-nvim-lsp-signature-help" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-path" },
            { "hrsh7th/cmp-cmdline" },
            { "ray-x/cmp-treesitter" },
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
            { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
        },
    },

    {
        "neovim/nvim-lspconfig",
        event = { "BufReadPre", "BufNewFile" },
    },

    -- Auto close parentheses
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {
            check_ts = true,
            ts_config = {
                lua = { 'string' }, -- it will not add a pair on that treesitter node
                javascript = { 'template_string' },
                java = false,       -- don't check treesitter on java
            },
        },
    },

    -- formatter
    {
        "stevearc/conform.nvim",
        event = { "BufWritePre" },
        keys = {
            {
                -- Customize or remove this keymap to your liking
                "<leader>bf",
                function()
                    require("conform").format({ async = true, lsp_fallback = true })
                end,
                mode = "",
                desc = "Buffer Format",
            },
        },
        -- Everything in opts will be passed to setup()
        opts = {
            -- Define your formatters
            formatters_by_ft = {
                sh = { 'shfmt' },
                sql = { 'sqlfluff' },
                json = { "jq" },
                yaml = { "prettier" },
                markdown = { "prettier" },
                proto = { "clang_format" },
                -- ["_"] = { "trim_whitespace" },
            },
            -- Set up format-on-save
            format_on_save = { timeout_ms = 500, lsp_fallback = true },
            -- Customize formatters
            formatters = {
                shfmt = {
                    prepend_args = { "-i", "4", "-ci", "-bn" },
                },
                prettier = {
                    options = {
                        ft_parsers = {
                            markdown = "markdown",
                            yaml = "yaml",
                        },
                    },
                },
            },
        },
        init = function()
            -- If you want the formatexpr, here is the place to set it
            vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
        end,
    },

    -- Whichkey
    {
        "folke/which-key.nvim",
        event = 'VimEnter',
        config = true,
    },

    -- statusline
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        opts = {
            sections = {
                lualine_a = {
                    { 'mode' },
                    {
                        'searchcount',
                        maxcount = 999,
                        timeout = 500,
                    },
                },
            },
            extensions = { "lazy", "toggleterm", "nvim-tree" },
        },
    },

    -- lua
    {
        "spacewander/openresty-vim",
        ft = { "nginx" },
    },

    {
        "b0o/schemastore.nvim",
    },

    -- http
    {
        'mistweaverco/kulala.nvim',
        ft = { "http" },
        config = function()
            require('kulala').setup()
        end,
        keys = {
            {
                "<leader>kr",
                function()
                    require("kulala").run()
                end,
                desc = "Run Request",
            },
        },
    },

    -- Terminal
    {
        "akinsho/toggleterm.nvim",
        event = 'CursorHold',
        opts = {
            open_mapping = [[<c-\>]],
            shade_filetypes = { "none" },
            direction = "horizontal",
            float_opts = { border = "curved" },
            size = function(term)
                if term.direction == "horizontal" then
                    return 15
                elseif term.direction == "vertical" then
                    return vim.o.columns * 0.4
                end
            end,
        },
    },
}, {})

---- Settings ----

--- Tab Configuration
local indent = 4
vim.opt.shiftwidth = indent
vim.opt.tabstop = indent
vim.opt.softtabstop = indent

vim.opt.smartindent = true
vim.opt.expandtab = true

-- Make line numbers default
vim.opt.number = true
--Set colorscheme (order is important here)
vim.opt.termguicolors = true
--Set highlight on search
vim.opt.showmatch = true

-- 光标设置成竖线
vim.opt.guicursor = 'a:ver25'
--Enable mouse mode
vim.opt.mouse = "a"
-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.opt.clipboard = 'unnamedplus'
vim.opt.mousefocus = true

-- Enable break indent
vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

---- Plugin Settings ----
-- 开启 Folding 模块 zc zo
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
-- 默认不要折叠
-- https://stackoverflow.com/questions/8316139/how-to-set-the-default-to-unfolded-when-you-open-a-file
vim.opt.foldlevel = 99

-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = 'yes'

-- Decrease update time
vim.opt.updatetime = 250

-- Decrease mapped sequence wait time
-- Displays which-key popup sooner
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Preview substitutions live, as you type!
vim.opt.inccommand = 'split'

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- Enable line wrapping
vim.opt.wrap = true

-- Enables syntax highlighting
vim.opt.syntax = 'enable'

-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Keybinds to make split navigation easier.
-- Use CTRL+<hjkl> to switch between windows
-- See `:help wincmd` for a list of all window commands
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

vim.filetype.add({
    extension = {
        ['http'] = 'http',
    },
})

-- LSP settings
local nvimLsp = require("lspconfig")

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local onAttach = function(client, bufnr)
    local map = function(keys, func, desc)
        vim.keymap.set('n', keys, func, { buffer = bufnr, desc = 'LSP: ' .. desc })
    end

    -- Mappings.
    -- Rename the variable under your cursor.
    --  Most Language Servers support renaming across files, etc.
    map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

    -- WARN: This is not Goto Definition, this is Goto Declaration.
    --  For example, in C this would take you to the header.
    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
        local highlight_augroup = vim.api.nvim_create_augroup('kickstart-lsp-highlight', { clear = false })
        vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
            buffer = bufnr,
            group = highlight_augroup,
            callback = vim.lsp.buf.document_highlight,
        })

        vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
            buffer = bufnr,
            group = highlight_augroup,
            callback = vim.lsp.buf.clear_references,
        })

        vim.api.nvim_create_autocmd('LspDetach', {
            group = vim.api.nvim_create_augroup('kickstart-lsp-detach', { clear = true }),
            callback = function(event)
                vim.lsp.buf.clear_references()
                vim.api.nvim_clear_autocmds { group = 'kickstart-lsp-highlight', buffer = event.buf }
            end,
        })
    end

    -- The following autocommand is used to enable inlay hints in your
    -- code, if the language server you are using supports them
    --
    -- This may be unwanted, since they displace some of your code
    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
        map('<leader>th', function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled {})
        end, '[T]oggle Inlay [H]ints')
    end
end

local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- 语言服务与相关设置
local servers = {
    bashls = {},
    neocmake = {},
    dockerls = {},
    dotls = {},
    gopls = {},
    jsonls = {
        json = {
            schemas = require('schemastore').json.schemas(),
            validate = { enable = true },
        },
    },
    yamlls = {
        yaml = {
            schemastore = {
                enable = true,
            },
            hover = true,
            completion = true,
            validate = true,
        },
    },
    clangd = {},
    ruff = {},
    marksman = {},
    lua_ls = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
            },
            completion = {
                callSnippet = 'Replace',
                displayContext = 1,
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
                neededFileStatus = {
                    ["codestyle-check"] = "Any",
                },
            },
            workspace = {
                checkThirdParty = false,
                library = {
                    vim.env.VIMRUNTIME,
                    -- Depending on the usage, you might want to add additional paths here.
                    -- "${3rd}/luv/library"
                    -- "${3rd}/busted/library",
                },
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
            format = {
                enable = true,
                -- Put format options here
                -- NOTE: the value should be STRING!!
                defaultConfig = {
                    indent_style = "space",
                    indent_size = "4",
                }
            },
        },
    },
}

for lsp, sets in pairs(servers) do
    nvimLsp[lsp].setup({
        on_attach = onAttach,
        capabilities = capabilities,
        settings = sets,
    })
end
