-- Install packer
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
end

vim.cmd([[packadd packer.nvim]])

-- Auto compile when there are changes
vim.cmd("autocmd BufWritePost init.lua PackerCompile")

---- Plugins ----
local packer = require("packer")
local use = packer.use
packer.reset()
packer.startup(function()
    use({ "wbthomason/packer.nvim" }) -- Package manager

    -- git
    use({
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup()
        end,
    })

    -- comment
    use({
        "terrortylor/nvim-comment",
        config = function()
            require("nvim_comment").setup()
        end,
    })

    -- UI to select things (files, grep results, open buffers...)
    use({ "nvim-lua/popup.nvim" })
    use({ "nvim-lua/plenary.nvim" })
    use({ "nvim-telescope/telescope.nvim" })

    use({
        "navarasu/onedark.nvim",
        config = function()
            require("onedark").setup()
        end,
    })

    -- icons
    use({
        "kyazdani42/nvim-web-devicons",
        config = function()
            require("nvim-web-devicons").setup({
                -- DevIcon will be appended to `name`
                override = {
                    zsh = {
                        icon = "",
                        color = "#428850",
                        name = "Zsh",
                    },
                },
                -- globally enable default icons (default to false)
                -- will get overriden by `get_icons` option
                default = true,
            })
        end,
    })

    -- Add indentation guides even on blank lines
    use({
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            vim.g.indent_blankline_char = "│"
        end,
    })

    -- Highlights
    use({
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
                highlight = {
                    enable = true, -- false will disable the whole extension
                },
                matchip = { enable = true },
                rainbow = {
                    enable = true,
                    extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
                    -- max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int
                },
            })
        end,
    })

    use({ "mhartington/formatter.nvim" })

    -- Completion and linting
    use({ "hrsh7th/nvim-compe" })
    use({ "L3MON4D3/LuaSnip" })
    use({ "neovim/nvim-lspconfig" })

    -- zsh
    use({ "tamago324/compe-zsh" })
    use({ "Shougo/deol.nvim" })

    -- Auto close parentheses
    use({
        "windwp/nvim-autopairs",
        config = function()
            require("nvim-autopairs").setup()
        end,
    })

    -- Lua
    use({
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup({})
        end,
    })

    -- statusline
    use({
        "glepnir/galaxyline.nvim",
        branch = "main",
        config = function()
            local gl = require("galaxyline")
            local gls = gl.section
            gl.short_line_list = { "LuaTree", "vista", "dbui" }

            local colors = {
                bg = "#282c34",
                yellow = "#fabd2f",
                cyan = "#008080",
                darkblue = "#081633",
                green = "#afd700",
                orange = "#FF8800",
                purple = "#5d4d7a",
                magenta = "#d16d9e",
                grey = "#c0c0c0",
                blue = "#0087d7",
                red = "#ec5f67",
            }

            local buffer_not_empty = function()
                if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
                    return true
                end
                return false
            end

            gls.left[1] = {
                FirstElement = {
                    provider = function()
                        return "▋"
                    end,
                    highlight = { colors.blue, colors.yellow },
                },
            }
            gls.left[2] = {
                ViMode = {
                    provider = function()
                        local alias = {
                            n = "NORMAL",
                            i = "INSERT",
                            c = "COMMAND",
                            v = "VISUAL",
                            V = "VISUAL LINE",
                            [""] = "VISUAL BLOCK",
                        }
                        return alias[vim.fn.mode()]
                    end,
                    separator = " ",
                    separator_highlight = {
                        colors.purple,
                        function()
                            if not buffer_not_empty() then
                                return colors.purple
                            end
                            return colors.darkblue
                        end,
                    },
                    highlight = { colors.darkblue, colors.purple, "bold" },
                },
            }
            gls.left[3] = {
                FileIcon = {
                    provider = "FileIcon",
                    condition = buffer_not_empty,
                    highlight = { require("galaxyline.provider_fileinfo").get_file_icon_color, colors.darkblue },
                },
            }
            gls.left[4] = {
                FileName = {
                    provider = { "FileName", "FileSize" },
                    condition = buffer_not_empty,
                    separator = "",
                    separator_highlight = { colors.purple, colors.darkblue },
                    highlight = { colors.magenta, colors.darkblue },
                },
            }

            gls.left[5] = {
                GitIcon = {
                    provider = function()
                        return "  "
                    end,
                    condition = buffer_not_empty,
                    highlight = { colors.orange, colors.purple },
                },
            }
            gls.left[6] = {
                GitBranch = {
                    provider = "GitBranch",
                    condition = buffer_not_empty,
                    highlight = { colors.grey, colors.purple },
                },
            }

            local checkwidth = function()
                local squeeze_width = vim.fn.winwidth(0) / 2
                if squeeze_width > 40 then
                    return true
                end
                return false
            end

            gls.left[7] = {
                DiffAdd = {
                    provider = "DiffAdd",
                    condition = checkwidth,
                    icon = "   ",
                    highlight = { colors.green, colors.purple },
                },
            }
            gls.left[8] = {
                DiffModified = {
                    provider = "DiffModified",
                    condition = checkwidth,
                    icon = " ",
                    highlight = { colors.orange, colors.purple },
                },
            }
            gls.left[9] = {
                DiffRemove = {
                    provider = "DiffRemove",
                    condition = checkwidth,
                    icon = " ",
                    highlight = { colors.red, colors.purple },
                },
            }
            gls.left[10] = {
                LeftEnd = {
                    provider = function()
                        return ""
                    end,
                    separator = "",
                    separator_highlight = { colors.purple, colors.bg },
                    highlight = { colors.purple, colors.purple },
                },
            }
            gls.left[11] = {
                DiagnosticError = {
                    provider = "DiagnosticError",
                    icon = "  ",
                    highlight = { colors.red, colors.bg },
                },
            }
            gls.left[12] = {
                Space = {
                    provider = function()
                        return " "
                    end,
                },
            }
            gls.left[13] = {
                DiagnosticWarn = {
                    provider = "DiagnosticWarn",
                    icon = "  ",
                    highlight = { colors.blue, colors.bg },
                },
            }

            gls.right[1] = {
                FileFormat = {
                    provider = "FileFormat",
                    separator = " ",
                    separator_highlight = { colors.bg, colors.purple },
                    highlight = { colors.grey, colors.purple },
                },
            }
            gls.right[2] = {
                LineInfo = {
                    provider = "LineColumn",
                    separator = " | ",
                    separator_highlight = { colors.darkblue, colors.purple },
                    highlight = { colors.grey, colors.purple },
                },
            }
            gls.right[3] = {
                PerCent = {
                    provider = "LinePercent",
                    separator = "",
                    separator_highlight = { colors.darkblue, colors.purple },
                    highlight = { colors.grey, colors.darkblue },
                },
            }
            gls.right[4] = {
                ScrollBar = {
                    provider = "ScrollBar",
                    highlight = { colors.yellow, colors.purple },
                },
            }

            gls.short_line_left[1] = {
                BufferType = {
                    provider = "FileTypeName",
                    separator = "",
                    separator_highlight = { colors.purple, colors.bg },
                    highlight = { colors.grey, colors.purple },
                },
            }

            gls.short_line_right[1] = {
                BufferIcon = {
                    provider = "BufferIcon",
                    separator = "",
                    separator_highlight = { colors.purple, colors.bg },
                    highlight = { colors.grey, colors.purple },
                },
            }
        end,
    })

    -- lua
    use({ "tjdevries/nlua.nvim" })
    use({ "spacewander/openresty-vim" })

    -- Terminal
    use({
        "numtostr/FTerm.nvim",
        config = function()
            require("FTerm").setup()
            local map = vim.api.nvim_set_keymap
            local opts = { noremap = true, silent = true }

            map("n", "<A-i>", '<CMD>lua require("FTerm").toggle()<CR>', opts)
            map("t", "<A-i>", '<C-\\><C-n><CMD>lua require("FTerm").toggle()<CR>', opts)
        end,
    })
end)

---- Settings ----

local o, wo, bo = vim.o, vim.wo, vim.bo
local indent = 4
-- Global Options
--Incremental live completion
o.inccommand = "nosplit"

--Set colorscheme (order is important here)
o.termguicolors = true
--Set highlight on search
o.showmatch = true
o.completeopt = "menuone,noinsert"

--Do not save when switching buffers
o.hidden = true
o.shortmess = o.shortmess .. "c"
o.guicursor = [[n-v-c:ver25,i-ci-ve:ver35,ve:ver35,i-ci:ver25,r-cr:hor20,o:hor50]]
--Decrease update time
o.updatetime = 250
--Case insensitive searching UNLESS /C or capital in search
o.ignorecase = true
o.smartcase = true
--Enable mouse mode
o.mouse = "a"
--Enable break indent
o.breakindent = true
--Save undo history
o.undofile = true

-- Buffer Local Options
bo.smartindent = true
bo.tabstop = indent
bo.shiftwidth = indent
bo.expandtab = true

-- Window Local Options
wo.signcolumn = "yes"
--Make line numbers default
wo.number = true
wo.cursorline = true

--Remap space as leader key
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

--Remap for dealing with word wrap
vim.api.nvim_set_keymap("n", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("n", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

---- Plugin Settings ----

-- Format
local function clangformat()
    return { exe = "clang-format", args = { "-assume-filename=" .. vim.fn.expand("%:t") }, stdin = true }
end

local function prettier()
    return {
        exe = "prettier",
        args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0) },
        stdin = true,
    }
end

local function shfmt()
    return { exe = "shfmt", args = { "-" }, stdin = true }
end

require("formatter").setup({
    logging = false,
    filetype = {
        c = { clangformat },
        cpp = { clangformat },
        json = { prettier },
        javascript = { prettier },
        yaml = { prettier },
        sh = { shfmt },
        dockerfile = { shfmt },
        lua = {
            -- stylua
            function()
                return { exe = "stylua", args = { "--indent-type=Spaces", "-" }, stdin = true }
            end,
        },
    },
})

vim.api.nvim_exec(
    [[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost * FormatWrite
augroup END
]],
    true
)

-- Telescope
require("telescope").setup({
    defaults = {
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
        prompt_prefix = "> ",
        selection_caret = "> ",
        entry_prefix = "  ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "horizontal",
        layout_config = {
            horizontal = {
                mirror = false,
            },
            vertical = {
                mirror = false,
            },
        },
        file_sorter = require("telescope.sorters").get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
        winblend = 0,
        border = {},
        borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        color_devicons = true,
        use_less = true,
        path_display = {},
        set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,

        -- Developer configurations: Not meant for general override
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
    },
})

--Add leader shortcuts
vim.api.nvim_set_keymap(
    "n",
    "<leader>ff",
    [[<cmd>lua require('telescope.builtin').find_files()<cr>]],
    { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
    "n",
    "<leader>fb",
    [[<cmd>lua require('telescope.builtin').buffers()<cr>]],
    { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
    "n",
    "<leader>fg",
    [[<cmd>lua require('telescope.builtin').live_grep()<cr>]],
    { noremap = true, silent = true }
)

-- Change preview window location
vim.g.splitbelow = true

-- LSP settings
local nvim_lsp = require("lspconfig")
local protocol = require("vim.lsp.protocol")

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    --Enable completion triggered by <c-x><c-o>
    buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

    -- Mappings.
    local opts = { noremap = true, silent = true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    --buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    --buf_set_keymap('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
    buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
    --buf_set_keymap('n', '<C-j>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap("n", "<S-C-j>", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

    -- formatting
    vim.cmd([[ command! Format execute 'lua vim.lsp.buf.formatting()' ]])

    if client.resolved_capabilities.document_highlight == true then
        vim.cmd("augroup lsp_aucmds")
        vim.cmd("au CursorHold <buffer> lua vim.lsp.buf.document_highlight()")
        vim.cmd("au CursorMoved <buffer> lua vim.lsp.buf.clear_references()")
        vim.cmd("augroup END")
    end

    -- require("completion").on_attach(client, bufnr)

    --protocol.SymbolKind = { }
    protocol.CompletionItemKind = {
        "", -- Text
        "", -- Method
        "", -- Function
        "", -- Constructor
        "", -- Field
        "", -- Variable
        "", -- Class
        "ﰮ", -- Interface
        "", -- Module
        "", -- Property
        "", -- Unit
        "", -- Value
        "", -- Enum
        "", -- Keyword
        "﬌", -- Snippet
        "", -- Color
        "", -- File
        "", -- Reference
        "", -- Folder
        "", -- EnumMember
        "", -- Constant
        "", -- Struct
        "", -- Event
        "ﬦ", -- Operator
        "", -- TypeParameter
    }
end

-- icon
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    underline = true,
    signs = true,
    -- This sets the spacing and the prefix, obviously.
    virtual_text = {
        spacing = 4,
        prefix = "",
    },
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local servers = { "pyright", "bashls", "dockerls", "dotls", "sqls", "gopls", "yamlls", "clangd" }

for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup({ on_attach = on_attach, capabilities = capabilities })
end

-- lua
local system_name
if fn.has("mac") == 1 then
    system_name = "macOS"
elseif fn.has("unix") == 1 then
    system_name = "Linux"
elseif fn.has("win32") == 1 then
    system_name = "Windows"
else
    print("Unsupported system for sumneko")
end

local sumneko_root_path = fn.getenv("HOME") .. "/myDoc/lua-language-server"
local sumneko_binary = sumneko_root_path .. "/bin/" .. system_name .. "/lua-language-server"

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

-- nvim_lsp.sumneko_lua.setup({
require("nlua.lsp.nvim").setup(nvim_lsp, {
    cmd = { sumneko_binary, "-E", sumneko_root_path .. "/main.lua" },
    on_attach = on_attach,
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

require("compe").setup({
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    throttle_time = 80,
    source_timeout = 200,
    resolve_timeout = 800,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = {
        border = { "", "", "", " ", "", "", "", " " }, -- the border option is the same as `|help nvim_open_win|`
        winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
        max_width = 120,
        min_width = 60,
        max_height = math.floor(vim.o.lines * 0.3),
        min_height = 1,
    },

    source = {
        path = true,
        zsh = true,
        buffer = true,
        calc = true,
        nvim_lsp = true,
        nvim_lua = true,
        luasnip = true,
        treesitter = true,
    },
})

-- Utility functions for compe and luasnip
local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col(".") - 1
    return col == 0 or vim.fn.getline("."):sub(col, col):match("%s") ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menu
--- jump to prev/next snippet's placeholder

local luasnip = require("luasnip")

_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t("<C-n>")
    elseif luasnip.expand_or_jumpable() then
        return t("<Plug>luasnip-expand-or-jump")
    elseif check_back_space() then
        return t("<Tab>")
    else
        return vim.fn["compe#complete"]()
    end
end

_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t("<C-p>")
    elseif luasnip.jumpable(-1) then
        return t("<Plug>luasnip-jump-prev")
    else
        return t("<S-Tab>")
    end
end

-- Map tab to the above tab complete functions
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })

-- Map compe confirm and complete functions
vim.api.nvim_set_keymap("i", "<cr>", 'compe#confirm("<cr>")', { expr = true })
vim.api.nvim_set_keymap("i", "<c-space>", "compe#complete()", { expr = true })
