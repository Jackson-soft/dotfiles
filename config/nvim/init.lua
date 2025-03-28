-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

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
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
-- 默认不要折叠
-- https://stackoverflow.com/questions/8316139/how-to-set-the-default-to-unfolded-when-you-open-a-file
vim.opt.foldlevel = 99

vim.opt.diffopt = "internal,filler,closeoff,indent-heuristic,linematch:60,algorithm:histogram"

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
        config = function()
            local configs = require("nvim-treesitter.configs")
            configs.setup({
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
                    "markdown_inline",
                    "proto",
                    "python",
                    "regex",
                    "sql",
                    "toml",
                    "typescript",
                    "yaml",
                },
                auto_install = true,
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
                    highlight_current_scope = { enable = true },
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
            })
        end,
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

    -- Completion and linting
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

    {
        'saghen/blink.cmp',
        lazy = false, -- lazy loading handled internally

        -- use a release tag to download pre-built binaries
        version = '1.*',
        opts = {
            sources = {
                -- add lazydev to your completion providers
                default = { "lazydev", "lsp", "path", "snippets", "buffer" },
                providers = {
                    -- dont show LuaLS require statements when lazydev has items
                    lazydev = {
                        name = "LazyDev",
                        module = "lazydev.integrations.blink",
                        score_offset = 100,
                    },
                },
            },
            -- 'default' for mappings similar to built-in completion
            -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
            -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
            -- see the "default configuration" section below for full documentation on how to define
            -- your own keymap.
            keymap = {
                preset = 'enter',
                ['<S-Tab>'] = { 'select_prev', 'snippet_backward', 'fallback' },
                ['<Tab>'] = { 'select_next', 'snippet_forward', 'fallback' },
            },

            -- experimental auto-brackets support
            completion = {
                documentation = { auto_show = true },
                ghost_text = {
                    enabled = true,
                },
                list = {
                    selection = {
                        preselect = function(ctx)
                            return ctx.mode ~= 'cmdline' and not require('blink.cmp').snippet_active({ direction = 1 })
                        end,
                    },
                },
            },

            -- experimental signature help support
            signature = { enabled = true }
        },
        opts_extend = { "sources.default" }
    },

    {
        "neovim/nvim-lspconfig",
        config = function()
            --  This function gets run when an LSP attaches to a particular buffer.
            --    That is to say, every time a new file is opened that is associated with
            --    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
            --    function will be executed to configure the current buffer
            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
                callback = function(event)
                    -- NOTE: Remember that Lua is a real programming language, and as such it is possible
                    -- to define small helper and utility functions so you don't have to repeat yourself.
                    --
                    -- In this case, we create a function that lets us more easily define mappings specific
                    -- for LSP related items. It sets the mode, buffer and description for us each time.
                    local map = function(keys, func, desc, mode)
                        mode = mode or 'n'
                        vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
                    end

                    -- Rename the variable under your cursor.
                    --  Most Language Servers support renaming across files, etc.
                    map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

                    -- Execute a code action, usually your cursor needs to be on top of an error
                    -- or a suggestion from your LSP for this to activate.
                    map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction', { 'n', 'x' })

                    -- WARN: This is not Goto Definition, this is Goto Declaration.
                    --  For example, in C this would take you to the header.
                    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

                    -- The following two autocommands are used to highlight references of the
                    -- word under your cursor when your cursor rests there for a little while.
                    --    See `:help CursorHold` for information about when this is executed
                    --
                    -- When you move your cursor, the highlights will be cleared (the second autocommand).
                    local client = vim.lsp.get_client_by_id(event.data.client_id)
                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
                        local highlight_augroup = vim.api.nvim_create_augroup('kickstart-lsp-highlight',
                            { clear = false })
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
                            group = vim.api.nvim_create_augroup('kickstart-lsp-detach', { clear = true }),
                            callback = function(event2)
                                vim.lsp.buf.clear_references()
                                vim.api.nvim_clear_autocmds { group = 'kickstart-lsp-highlight', buffer = event2.buf }
                            end,
                        })
                    end

                    -- The following code creates a keymap to toggle inlay hints in your
                    -- code, if the language server you are using supports them
                    --
                    -- This may be unwanted, since they displace some of your code
                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
                        map('<leader>th', function()
                            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
                        end, '[T]oggle Inlay [H]ints')
                    end
                end,
            })

            -- Enable the following language servers
            -- 语言服务与相关设置
            local servers = {
                bashls = {},
                buf_ls = {},
                neocmake = {},
                dockerls = {},
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
            local nvimLsp = require("lspconfig")
            local capabilities = require('blink.cmp').get_lsp_capabilities()

            for lsp, sets in pairs(servers) do
                nvimLsp[lsp].setup({
                    capabilities = capabilities,
                    settings = sets,
                })
            end

            nvimLsp.dotls.setup({
                cmd = { 'dot-ls' },
                capabilities = capabilities,
            })
        end,
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
                desc = "[B]uffer [F]ormat",
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
        "MeanderingProgrammer/render-markdown.nvim",
        opts = {},
    },

    {
        "b0o/schemastore.nvim",
    },

    -- http
    {
        'mistweaverco/kulala.nvim',
        ft = "http",
        init = function()
            vim.filetype.add({
                extension = { ["http"] = "http" },
            })
        end,
        keys = function()
            local kulala = require("kulala")
            return {
                { "<leader>rr", kulala.run,         desc = "Send the request" },
                { "<leader>rt", kulala.toggle_view, desc = "Toggle headers/body" },
                { "<leader>rp", kulala.jump_prev,   desc = "Jump to previous request" },
                { "<leader>rn", kulala.jump_next,   desc = "Jump to next request" },
            }
        end,
        opts = {},
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
