-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        lazypath,
    })
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
        cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewToggleFiles", "DiffviewFocusFiles" },
        config = true,
        keys = {
            { "<leader>dv", "<cmd>DiffviewOpen<cr>",  desc = "DiffView Open" },
            { "<leader>dc", "<cmd>DiffviewClose<cr>", desc = "DiffView Close" },
        },
    },

    {
        "TimUntersberger/neogit",
        lazy = true,
        opts = {
            integrations = {
                diffview = true,
            },
        },
        keys = {
            { "<leader>gg", "<Cmd>Neogit<CR>", desc = "Open Neogit" },
        },
    },

    -- comment
    {
        "numToStr/Comment.nvim",
        event = 'BufRead',
        config = true
    },

    -- file tree
    {
        "nvim-tree/nvim-tree.lua",
        event = 'CursorHold',
        cmd = {
            "NvimTreeToggle",
            "NvimTreeOpen",
            "NvimTreeFindFile",
            "NvimTreeFindFileToggle",
            "NvimTreeRefresh",
        },
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
        "nvim-telescope/telescope.nvim",
        event = 'CursorHold',
        config = function()
            local telescope = require("telescope")
            telescope.setup({
                defaults = {
                    mappings = {
                        i = {
                            -- map actions.which_key to <C-h> (default: <C-/>)
                            ["<C-h>"] = "which_key"
                        }
                    }
                },
                extensions = {
                    fzf = {
                        fuzzy = true,                   -- false will only do exact matching
                        override_generic_sorter = true, -- override the generic sorter
                        override_file_sorter = true,    -- override the file sorter
                    },
                },
            })
            telescope.load_extension("fzf")
            telescope.load_extension("file_browser")

            local builtin = require('telescope.builtin')
            vim.keymap.set('n', '<leader>?', builtin.oldfiles, { desc = '[?] Find recently opened files' })
            vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = '[S]earch [F]iles' })
            vim.keymap.set('n', '<leader>sg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
            vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Find existing buffers' })
            vim.keymap.set('n', '<leader>sh', builtin.help_tags, { desc = '[S]earch [H]elp' })
            vim.keymap.set("n", "<leader>ds", builtin.lsp_document_symbols, { desc = "[D]ocument [S]ymbols" })
            vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
            vim.keymap.set('n', '<leader>sd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
        end,
        dependencies = {
            { "nvim-telescope/telescope-fzf-native.nvim",   build = 'make' },
            { "nvim-telescope/telescope-file-browser.nvim", lazy = true },
        },
    },

    -- Themes
    {
        "folke/tokyonight.nvim",
        priority = 1000,
        config = function()
            vim.cmd.colorscheme 'tokyonight-night'
        end,
    },

    -- Add indentation guides even on blank lines
    {
        "lukas-reineke/indent-blankline.nvim",
        event = { "BufReadPost", "BufNewFile" },
        opts = {
            show_current_context = true,
            show_current_context_start = true,
        },
        config = function()
            vim.opt.list = true
        end,
    },

    -- Highlights
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        event = { "BufReadPost", "BufNewFile" },
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
                "diff",
                "gitignore",
                "go",
                "gomod",
                "gowork",
                "html",
                "http",
                "javascript",
                "json",
                "jsonc",
                "lua",
                "make",
                "markdown",
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

    -- Completion and linting
    {
        "hrsh7th/nvim-cmp",
        config = function()
            local cmp = require("cmp")
            local luasnip = require("luasnip")

            local has_words_before = function()
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0
                    and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") ==
                    nil
            end

            cmp.setup({
                snippet = {
                    expand = function(args)
                        luasnip.lsp_expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ['<C-n>'] = cmp.mapping.select_next_item(),
                    ['<C-p>'] = cmp.mapping.select_prev_item(),
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true }),
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_next_item()
                        elseif luasnip.expand_or_jumpable() then
                            luasnip.expand_or_jump()
                        elseif has_words_before() then
                            cmp.complete()
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                            luasnip.jump(-1)
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp" },
                    { name = "nvim_lua" },
                    { name = "luasnip" },
                    { name = "buffer" },
                    { name = "path" },
                    { name = "treesitter" },
                    { name = "zsh" },
                    { name = 'nvim_lsp_signature_help' },
                }),
            })

            -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
            cmp.setup.cmdline({ '/', '?' }, {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'nvim_lsp_document_symbol' }
                }, {
                    { name = 'buffer' }
                })
            })

            -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
            cmp.setup.cmdline(':', {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'path' }
                }, {
                    { name = 'cmdline' }
                })
            })
        end,
        dependencies = {
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-nvim-lsp-signature-help" },
            { "hrsh7th/cmp-nvim-lsp-document-symbol" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-path" },
            { "hrsh7th/cmp-cmdline" },
            { "ray-x/cmp-treesitter" },
            {
                "saadparwaiz1/cmp_luasnip",
                dependencies = { "L3MON4D3/LuaSnip" },
            },
            {
                "tamago324/cmp-zsh",
                dependencies = { "Shougo/deol.nvim" },
                opts = {
                    zshrc = true,                      -- Source the zshrc (adding all custom completions). default: false
                    filetypes = { "deoledit", "zsh" }, -- Filetypes to enable cmp_zsh source. default: {"*"}
                },
            },
        },
    },

    {
        "neovim/nvim-lspconfig",
        event = { "BufReadPre", "BufNewFile" },
    },

    {
        "jose-elias-alvarez/null-ls.nvim",
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

    -- Whichkey
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
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
        lazy = true,
    },

    -- http
    {
        "NTBBloodbath/rest.nvim",
        ft = { "http" },
        keys = {
            { "<leader>rt", "<cmd>RestNvim<cr>", desc = "rest neovim" },
        },
        opts = {
            result_split_horizontal = true,
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

--- Numbering
vim.wo.number = true
--Set colorscheme (order is important here)
vim.opt.termguicolors = true
--Set highlight on search
vim.opt.showmatch = true

vim.opt.guicursor = [[n-v-c:ver25,i-ci-ve:ver35,ve:ver35,i-ci:ver25,r-cr:hor20,o:hor50]]
--Enable mouse mode
vim.opt.mouse = "a"
-- Sync clipboard between OS and Neovim.
vim.o.clipboard = 'unnamedplus'
vim.opt.mousefocus = true
vim.opt.inccommand = "split"
-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeout = true
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Make line numbers default
vim.opt.cursorline = true

-- Change preview window location
vim.g.splitbelow = true

---- Plugin Settings ----
-- 开启 Folding 模块 zc zo
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
-- 默认不要折叠
-- https://stackoverflow.com/questions/8316139/how-to-set-the-default-to-unfolded-when-you-open-a-file
vim.opt.foldlevel = 99

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})

-- LSP settings
local nvimLsp = require("lspconfig")

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local onAttach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr, desc = '[G]oto [D]eclaration' })
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr, desc = '[G]oto [D]efinition' })
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = bufnr, desc = 'Hover Documentation' })
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr, desc = '[G]oto [I]mplementation' })
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, { buffer = bufnr, desc = 'Signature Documentation' })
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, {
        buffer = bufnr,
        desc =
        '[W]orkspace [A]dd Folder'
    })
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder,
        { buffer = bufnr, desc = '[W]orkspace [R]emove Folder' })
    vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, { buffer = bufnr, desc = '[W]orkspace [L]ist Folders' })
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, { buffer = bufnr, desc = 'Type [D]efinition' })
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, { buffer = bufnr, desc = '[R]e[n]ame' })
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, { buffer = bufnr, desc = '[C]ode [A]ction' })
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = bufnr, desc = '[G]oto [R]eferences' })
    -- vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)

    if client.server_capabilities.documentFormattingProvider then
        vim.api.nvim_create_autocmd("BufWritePre", {
            group = vim.api.nvim_create_augroup("Format", { clear = true }),
            buffer = bufnr,
            callback = function()
                -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
                -- vim.lsp.buf.formatting_sync()
                vim.lsp.buf.format({ async = true })
            end,
        })
    end
end

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = true,
    severity_sort = true,
})

local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- 语言服务与相关设置
local servers = {
    bashls = {},
    bufls = {},
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
                url = "https://www.schemastore.org/api/json/catalog.json",
            },
            schemas = require('schemastore').yaml.schemas(),
            hover = true,
            completion = true,
            validate = true,
        },
    },
    clangd = {},
    pyright = {},
    marksman = {},
    lua_ls = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
                neededFileStatus = {
                    ["codestyle-check"] = "Any",
                },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
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

local nullLs = require("null-ls")
nullLs.setup({
    on_attach = onAttach,
    -- register any number of sources simultaneously
    sources = {
        nullLs.builtins.formatting.buf,
        nullLs.builtins.formatting.prettier,
        nullLs.builtins.formatting.shfmt,
        -- nullLs.builtins.formatting.pg_format,
        nullLs.builtins.formatting.sqlfluff.with({
            extra_args = { "--dialect", "mysql" }, -- change to your dialect
        }),
        nullLs.builtins.formatting.ruff,

        nullLs.builtins.diagnostics.hadolint,
        nullLs.builtins.diagnostics.shellcheck,
        nullLs.builtins.diagnostics.markdownlint,
        nullLs.builtins.diagnostics.golangci_lint,
        nullLs.builtins.diagnostics.yamllint,
        nullLs.builtins.diagnostics.buf,
        nullLs.builtins.diagnostics.zsh,
        nullLs.builtins.diagnostics.ruff,
        nullLs.builtins.diagnostics.sqlfluff.with({
            extra_args = { "--dialect", "mysql" }, -- change to your dialect
        }),

        nullLs.builtins.code_actions.gitsigns,
        nullLs.builtins.code_actions.shellcheck,
    },
})
