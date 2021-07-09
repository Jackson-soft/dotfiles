require("lualine").setup({
	options = {
		icons_enabled = true,
		theme = "gruvbox",
		lower = true,
		component_separators = { "", "" },
		section_separators = { "", "" },
		disabled_filetypes = {},
	},
	sections = {
		lualine_a = { "mode" },
		lualine_b = { "branch" },
		lualine_c = { { "filename", file_status = true } },
		lualine_x = {
			{
				"diagnostics",
				sources = { "nvim_lsp" },
				symbols = { error = " ", warn = " ", info = " ", hint = " " },
			},
			"encoding",
			{ "filetype", colored = true },
		},
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = { "filename" },
		lualine_x = { "location" },
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	extensions = { "fugitive" },
})
