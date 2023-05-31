-- Pull in the wezterm API
local wezterm = require('wezterm')

-- Startup
wezterm.on('gui-startup', function(cmd)
    local _, _, window = wezterm.mux.spawn_window(cmd or {})
    window:gui_window():maximize()
end)

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

-- color scheme
config.color_scheme = 'tokyonight_moon'

config.default_cursor_style = 'BlinkingBar'

-- Font
config.font = wezterm.font('FiraCode Nerd Font', { weight = 'Bold' })
config.font_size = 17

-- Window
config.native_macos_fullscreen_mode = true
config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}


-- Tab bar
config.hide_tab_bar_if_only_one_tab = true

-- and finally, return the configuration to wezterm
return config
