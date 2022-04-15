-- Install packer
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
end

vim.cmd([[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]])

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
        branch = "main",
        config = function()
            require("gitsigns").setup({
                numhl = true,
                signs = {
                    add = { hl = "GitSignsAdd", text = "+", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn" },
                    change = {
                        hl = "GitSignsChange",
                        text = "│",
                        numhl = "GitSignsChangeNr",
                        linehl = "GitSignsChangeLn",
                    },
                    delete = {
                        hl = "GitSignsDelete",
                        text = "_",
                        numhl = "GitSignsDeleteNr",
                        linehl = "GitSignsDeleteLn",
                    },
                    topdelete = {
                        hl = "GitSignsDelete",
                        text = "‾",
                        numhl = "GitSignsDeleteNr",
                        linehl = "GitSignsDeleteLn",
                    },
                    changedelete = {
                        hl = "GitSignsChange",
                        text = "~",
                        numhl = "GitSignsChangeNr",
                        linehl = "GitSignsChangeLn",
                    },
                },
                on_attach = function(bufnr)
                    local function map(mode, lhs, rhs, opts)
                        opts = vim.tbl_extend("force", { noremap = true, silent = true }, opts or {})
                        vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
                    end

                    -- Navigation
                    map("n", "]c", "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", { expr = true })
                    map("n", "[c", "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", { expr = true })

                    -- Actions
                    map("n", "<leader>hs", ":Gitsigns stage_hunk<CR>")
                    map("v", "<leader>hs", ":Gitsigns stage_hunk<CR>")
                    map("n", "<leader>hr", ":Gitsigns reset_hunk<CR>")
                    map("v", "<leader>hr", ":Gitsigns reset_hunk<CR>")
                    map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>")
                    map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>")
                    map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>")
                    map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>")
                    map("n", "<leader>hb", '<cmd>lua require"gitsigns".blame_line{full=true}<CR>')
                    map("n", "<leader>tb", "<cmd>Gitsigns toggle_current_line_blame<CR>")
                    map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>")
                    map("n", "<leader>hD", '<cmd>lua require"gitsigns".diffthis("~")<CR>')
                    map("n", "<leader>td", "<cmd>Gitsigns toggle_deleted<CR>")

                    -- Text object
                    map("o", "ih", ":<C-U>Gitsigns select_hunk<CR>")
                    map("x", "ih", ":<C-U>Gitsigns select_hunk<CR>")
                end,
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
            require("Comment").setup()
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

            --Add leader shortcuts
            vim.api.nvim_set_keymap(
                "n",
                "<leader>sf",
                "<cmd>lua require('telescope.builtin').find_files()<cr>",
                { noremap = true, silent = true }
            )
            vim.api.nvim_set_keymap(
                "n",
                "<leader>sg",
                "<cmd>lua require('telescope.builtin').live_grep()<cr>",
                { noremap = true, silent = true }
            )
            vim.api.nvim_set_keymap("n", "<leader>fb", ":Telescope file_browser<cr>", {
                noremap = true,
                silent = true,
            })
        end,
        requires = {
            { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
            "nvim-telescope/telescope-file-browser.nvim",
        },
    })

    -- Themes
    use({
        "navarasu/onedark.nvim",
        config = function()
            local onedark = require("onedark")
            onedark.setup({
                style = "cool",
            })
            onedark.load()
        end,
    })

    -- Shade
    use({
        "sunjon/shade.nvim",
        config = function()
            require("shade").setup({
                overlay_opacity = 50,
                opacity_step = 1,
                keys = {
                    brightness_up = "<C-Up>",
                    brightness_down = "<C-Down>",
                    toggle = "<Leader>s",
                },
            })
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

            vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>Trouble<cr>", { silent = true, noremap = true })
            vim.api.nvim_set_keymap("n", "<leader>xq", "<cmd>Trouble quickfix<cr>", { silent = true, noremap = true })
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
                    end, {
                        "i",
                        "s",
                    }),

                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                            luasnip.jump(-1)
                        else
                            fallback()
                        end
                    end, {
                        "i",
                        "s",
                    }),
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

            vim.api.nvim_set_keymap("n", "<Leader>rt", "<Plug>RestNvim", { noremap = false })
            vim.api.nvim_set_keymap("n", "<Leader>rp", "<Plug>RestNvimPreview", { noremap = false })
            vim.api.nvim_set_keymap("n", "<Leader>rl", "<Plug>RestNvimLast", { noremap = false })
        end,
    })

    -- go
    use({
        "ray-x/go.nvim",
        config = function()
            require("go").setup({
                gofmt = "gopls",
            })
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
                        return 12
                    elseif term.direction == "vertical" then
                        return math.floor(vim.o.columns * 0.4)
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

--- Numbering {{{
vim.opt.number = true
--- }}}
--Set colorscheme (order is important here)
vim.opt.termguicolors = true
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
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "
-- Change preview window location
vim.g.splitbelow = true

-- Highlight on yank
vim.cmd([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]])

---- Plugin Settings ----

-- LSP settings
local nvim_lsp = require("lspconfig")

local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
vim.api.nvim_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
vim.api.nvim_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
vim.api.nvim_set_keymap("n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
    vim.api.nvim_buf_set_keymap(
        bufnr,
        "n",
        "<space>wl",
        "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
        opts
    )
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

    if client.resolved_capabilities.document_formatting then
        vim.cmd([[
            augroup LspFormatting
                autocmd! * <buffer>
                autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
            augroup END
            ]])
    end

    require("lsp_signature").on_attach()

    if client.resolved_capabilities.document_highlight then
        vim.cmd([[
            hi LspReferenceRead  gui=bold guibg=#1b1b29 blend=10
            hi LspReferenceText  gui=bold guibg=#1b1b29 blend=10
            hi LspReferenceWrite gui=bold guibg=#1b1b29 blend=10
            augroup lsp_document_highlight
            autocmd! * <buffer>
            autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
            autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
            augroup END
        ]])
    end
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    underline = true,
    update_in_insert = false,
    signs = true,
    virtual_text = { spacing = 4, prefix = "●" },
    severity_sort = true,
})

local signs = { Error = "", Warn = "", Hint = "", Info = "" }

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
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.formatting.stylua.with({
            args = { "--indent-type=Spaces", "-" },
        }),
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.cmake_format.with({
            extra_args = { "--tab-size=4" },
        }),
        null_ls.builtins.formatting.sqlformat.with({
            command = "pg_format",
            args = { "-" },
        }),
        null_ls.builtins.formatting.black,

        null_ls.builtins.diagnostics.ansiblelint,
        null_ls.builtins.diagnostics.selene,
        null_ls.builtins.diagnostics.hadolint,
        null_ls.builtins.diagnostics.shellcheck,
        null_ls.builtins.diagnostics.markdownlint,
        null_ls.builtins.diagnostics.golangci_lint,
        null_ls.builtins.diagnostics.yamllint.with({
            command = "js-yaml",
            args = { "-" },
        }),

        null_ls.builtins.code_actions.gitsigns,
    },
})

-- lua
local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

nvim_lsp.sumneko_lua.setup({
    on_attach = function(client, bufnr)
        on_attach(client, bufnr)

        client.resolved_capabilities.document_formatting = false
        client.resolved_capabilities.document_range_formatting = false
    end,
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
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
})
