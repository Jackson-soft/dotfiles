-- Install packer
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
    command = "source <afile> | PackerCompile",
    group = packer_group,
    pattern = "init.lua",
})

---- Plugins ----
local packer = require("packer")
local use = packer.use
packer.startup(function()
    -- Package manager
    use({ "wbthomason/packer.nvim" })

    -- Some requied Lua plugins
    use({ "nvim-lua/plenary.nvim" })

    -- git
    use({
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup({
                numhl = true,
                signs = {
                    add = { text = "+" },
                    change = { text = "│" },
                    delete = { text = "_" },
                    topdelete = { text = "‾" },
                    changedelete = { text = "~" },
                },
            })
        end,
    })

    use({
        "sindrets/diffview.nvim",
        config = function()
            require("diffview").setup({})
        end,
    })

    use({
        "TimUntersberger/neogit",
        config = function()
            local neogit = require("neogit")
            neogit.setup({
                use_magit_keybindings = true,
                integrations = {
                    diffview = true,
                },
            })
        end,
    })

    -- comment
    use({
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup({})
        end,
    })

    -- project management
    use({
        "ahmedkhalf/project.nvim",
        config = function()
            require("project_nvim").setup()
        end,
    })

    -- UI to select things (files, grep results, open buffers...)
    use({
        "nvim-telescope/telescope.nvim",
        config = function()
            local telescope = require("telescope")
            telescope.setup({
                extensions = {
                    fzf = {
                        fuzzy = true, -- false will only do exact matching
                        override_generic_sorter = true, -- override the generic sorter
                        override_file_sorter = true, -- override the file sorter
                        case_mode = "smart_case", -- or "ignore_case" or "respect_case"
                        -- the default case_mode is "smart_case"
                    },
                },
            })
            telescope.load_extension("fzf")
            telescope.load_extension("file_browser")

            vim.keymap.set("n", "<leader><space>", require("telescope.builtin").buffers)
            vim.keymap.set("n", "<leader>sf", require("telescope.builtin").find_files)
            vim.keymap.set("n", "<leader>sb", require("telescope.builtin").current_buffer_fuzzy_find)
            vim.keymap.set("n", "<leader>?", require("telescope.builtin").oldfiles)
        end,
        requires = {
            { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
            { "nvim-telescope/telescope-file-browser.nvim" },
        },
    })

    -- Themes
    use({
        "navarasu/onedark.nvim",
        config = function()
            local onedark = require("onedark")
            onedark.setup({
                style = "deep",
            })
            onedark.load()
        end,
    })

    -- icons
    use({
        "kyazdani42/nvim-web-devicons",
        config = function()
            require("nvim-web-devicons").setup()
        end,
    })

    -- Add indentation guides even on blank lines
    use({
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("indent_blankline").setup({
                buftype_exclude = { "terminal" },
            })
        end,
    })

    -- Highlights
    use({
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = {
                    "bash",
                    "c",
                    "cmake",
                    "comment",
                    "cpp",
                    "css",
                    "dockerfile",
                    "dot",
                    "go",
                    "gomod",
                    "html",
                    "http",
                    "javascript",
                    "json",
                    "jsonc",
                    "lua",
                    "make",
                    "markdown",
                    "ninja",
                    "proto",
                    "python",
                    "regex",
                    "toml",
                    "typescript",
                    "yaml",
                },
                highlight = {
                    enable = true, -- false will disable the whole extension
                    language_tree = true,
                },
                incremental_selection = {
                    enable = true,
                    keymaps = {
                        init_selection = "gnn",
                        node_incremental = "grn",
                        scope_incremental = "grc",
                        node_decremental = "grm",
                    },
                },
                indent = {
                    enable = true,
                },
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
            })
        end,

        requires = {
            { "nvim-treesitter/nvim-treesitter-refactor" },
            { "nvim-treesitter/nvim-treesitter-textobjects" },
        },
    })

    use({
        "onsails/lspkind-nvim",
        config = function()
            require("lspkind").init({
                preset = "codicons",
                symbol_map = {
                    Text = "",
                    Method = "",
                    Function = "",
                    Constructor = "",
                    Field = "ﰠ",
                    Variable = "",
                    Class = "ﴯ",
                    Interface = "",
                    Module = "",
                    Property = "ﰠ",
                    Unit = "塞",
                    Value = "",
                    Enum = "",
                    Keyword = "",
                    Snippet = "",
                    Color = "",
                    File = "",
                    Reference = "",
                    Folder = "",
                    EnumMember = "",
                    Constant = "",
                    Struct = "פּ",
                    Event = "",
                    Operator = "",
                    TypeParameter = "",
                },
            })
        end,
    })

    use({
        "folke/trouble.nvim",
        config = function()
            require("trouble").setup({
                auto_close = true,
                use_diagnostic_signs = true,
            })

            vim.keymap.set("n", "<leader>xx", "<cmd>Trouble<cr>", { silent = true })
            vim.keymap.set("n", "<leader>xq", "<cmd>Trouble quickfix<cr>", { silent = true })
        end,
    })

    -- Completion and linting
    use({
        "hrsh7th/nvim-cmp",
        config = function()
            local cmp = require("cmp")
            local luasnip = require("luasnip")
            local lspkind = require("lspkind")

            local has_words_before = function()
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0
                    and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
            end

            cmp.setup({
                snippet = {
                    expand = function(args)
                        luasnip.lsp_expand(args.body)
                    end,
                },

                mapping = cmp.mapping.preset.insert({
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),

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

                formatting = {
                    format = lspkind.cmp_format({
                        with_text = true,
                        menu = {
                            buffer = "[﬘ Buf]",
                            nvim_lsp = "[ LSP]",
                            luasnip = "[ LSnip]",
                            nvim_lua = "[ NvimLua]",
                            latex_symbols = "[ Latex]",
                            rg = "[ RG]",
                        },
                    }),
                },

                sources = cmp.config.sources({
                    { name = "buffer" },
                    { name = "nvim_lsp" },
                    { name = "nvim_lua" },
                    { name = "luasnip" },
                    { name = "path" },
                    { name = "treesitter" },
                    { name = "zsh" },
                    {
                        name = "dictionary",
                        keyword_length = 2,
                    },
                }),
            })
        end,
        requires = {
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-path" },
            { "ray-x/cmp-treesitter" },
            { "saadparwaiz1/cmp_luasnip" },
            { "L3MON4D3/LuaSnip" },
            {
                "tamago324/cmp-zsh",
                config = function()
                    require("cmp_zsh").setup({
                        zshrc = true, -- Source the zshrc (adding all custom completions). default: false
                        filetypes = { "deoledit", "zsh" }, -- Filetypes to enable cmp_zsh source. default: {"*"}
                    })
                end,
            },
            { "Shougo/deol.nvim" },
        },
    })
    use({
        "uga-rosa/cmp-dictionary",
        config = function()
            require("cmp_dictionary").setup({
                dic = {
                    ["*"] = { "/usr/share/dict/words" },
                },
            })
        end,
    })

    use({
        "neovim/nvim-lspconfig",
    })

    use({
        "ray-x/lsp_signature.nvim",
    })

    use({
        "jose-elias-alvarez/null-ls.nvim",
    })

    -- Auto close parentheses
    use({
        "windwp/nvim-autopairs",
        config = function()
            require("nvim-autopairs").setup({
                check_ts = true,
                ts_config = {
                    lua = { "string", "source" },
                    javascript = { "string", "template_string" },
                    java = false,
                },
            })
        end,
    })

    -- Whichkey
    use({
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup()
        end,
    })

    -- statusline
    use({
        "nvim-lualine/lualine.nvim",
        config = function()
            require("lualine").setup({
                options = { theme = "onedark" },
                extensions = { "toggleterm" },
            })
        end,
    })

    -- lua
    use({ "spacewander/openresty-vim" })

    -- http
    use({
        "NTBBloodbath/rest.nvim",
        config = function()
            require("rest-nvim").setup({
                result_split_horizontal = true,
            })

            vim.keymap.set("n", "<Leader>rt", "<Plug>(RestNvim)")
            vim.keymap.set("n", "<Leader>rp", "<Plug>i(RestNvimPreview)")
            vim.keymap.set("n", "<Leader>rl", "<Plug>(RestNvimLast)")
        end,
    })


    -- Terminal
    use({
        "akinsho/toggleterm.nvim",
        config = function()
            require("toggleterm").setup({
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
            })
        end,
    })
end)

---- Settings ----

--- Tab Configuration {{{
local indent = 4
vim.opt.shiftwidth = indent
vim.opt.tabstop = indent
vim.opt.softtabstop = indent

vim.opt.smartindent = true
vim.opt.expandtab = true
--- }}}

--- Numbering
vim.wo.number = true
--Set colorscheme (order is important here)
vim.o.termguicolors = true
--Set highlight on search
vim.opt.showmatch = true
vim.opt.completeopt = "menu,menuone,noselect"

vim.opt.guicursor = [[n-v-c:ver25,i-ci-ve:ver35,ve:ver35,i-ci:ver25,r-cr:hor20,o:hor50]]
--Enable mouse mode
vim.opt.mouse = "a"
vim.opt.mousefocus = true
vim.opt.inccommand = "split"
-- Undo
vim.opt.undofile = true

-- Update times
vim.opt.updatetime = 150

--Make line numbers default
vim.opt.cursorline = true

--Remap space as leader key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "
-- Change preview window location
vim.g.splitbelow = true

---- Plugin Settings ----

-- LSP settings
local nvim_lsp = require("lspconfig")

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    -- vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
    vim.keymap.set("n", "<leader>so", require("telescope.builtin").lsp_document_symbols, opts)

    if client.server_capabilities.documentFormattingProvider then
        local gLspFormat = vim.api.nvim_create_augroup("LspFormat", { clear = false })
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            group = gLspFormat,
            callback = vim.lsp.buf.formatting_sync,
        })
    end

    require("lsp_signature").on_attach()

    vim.api.nvim_create_autocmd("CursorHold", {
        buffer = bufnr,
        callback = function()
            local option = {
                focusable = false,
                close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
                border = 'rounded',
                source = 'always',
                prefix = ' ',
                scope = 'cursor',
            }
            vim.diagnostic.open_float(nil, option)
        end
    })

    if client.server_capabilities.documentHighlightProvider then
        vim.cmd [[
    hi! LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
    hi! LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
    hi! LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
  ]]
        vim.api.nvim_create_augroup('lsp_document_highlight', {
            clear = false
        })
        vim.api.nvim_clear_autocmds({
            buffer = bufnr,
            group = 'lsp_document_highlight',
        })
        vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
            group = 'lsp_document_highlight',
            buffer = bufnr,
            callback = vim.lsp.buf.document_highlight,
        })
        vim.api.nvim_create_autocmd('CursorMoved', {
            group = 'lsp_document_highlight',
            buffer = bufnr,
            callback = vim.lsp.buf.clear_references,
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

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

local servers = { "pyright", "bashls", "dockerls", "dotls", "gopls", "yamlls", "clangd", "jsonls" }

for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup({
        on_attach = on_attach,
        capabilities = capabilities,
    })
end

local null_ls = require("null-ls")
null_ls.setup({
    on_attach = on_attach,
    -- register any number of sources simultaneously
    sources = {
        null_ls.builtins.formatting.buf,
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.cmake_format.with({
            extra_args = { "--tab-size=4" },
        }),
        null_ls.builtins.formatting.pg_format,
        null_ls.builtins.formatting.black,

        null_ls.builtins.diagnostics.hadolint,
        null_ls.builtins.diagnostics.shellcheck,
        null_ls.builtins.diagnostics.markdownlint,
        null_ls.builtins.diagnostics.golangci_lint,
        null_ls.builtins.diagnostics.yamllint.with({
            command = "js-yaml",
            args = { "-" },
        }),
        null_ls.builtins.diagnostics.buf,
        null_ls.builtins.diagnostics.zsh,

        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.code_actions.shellcheck,
    },
})

-- lua
local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

nvim_lsp.sumneko_lua.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
                -- Setup your lua path
                path = runtime_path,
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
})
