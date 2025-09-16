-- Initialize Configuratio
local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux
local opacity = 0.85

config.automatically_reload_config = true
config.check_for_updates = true

--- Get the current operating system
--- @return "windows"| "linux" | "macos" | "unknown"
local function get_os()
	local target = wezterm.target_triple
	if target == "x86_64-pc-windows-msvc" then
		return "windows"
	elseif target == "x86_64-unknown-linux-gnu" then
		return "linux"
	elseif target == "x86_64-apple-darwin" or target == "aarch64-apple-darwin" then
		return "macos"
	end
	return "unknown"
end

local host_os = get_os()

wezterm.log_info("Detected OS:" .. host_os)

-- OS-Specific Overrides
if host_os == "linux" then
	config.default_prog = { "zsh" }
	config.front_end = "WebGpu"
	config.window_decorations = nil -- use system decorations
elseif host_os == "macos" then
	opacity = 0.85
	config.default_prog = { "zsh" }
	config.default_domain = nil
	config.window_decorations = "RESIZE"
	config.macos_window_background_blur = 50
elseif host_os == "windows" then
	-- Default Shell Configuration
	config.default_prog = { "pwsh", "-NoLogo" }
	config.wsl_domains = {
		{
			name = "WSL:FedoraLinux-42",
			distribution = "FedoraLinux-42",
			username = "paulinux",
			default_cwd = "/home/paulinux/",
		},
	}
	config.default_domain = "WSL:FedoraLinux-42"
end

-- Font Configuration
local emoji_font = "Segoe UI Emoji"
config.font = wezterm.font_with_fallback({
	"CommitMonoVoid",
	"Symbols Nerd Font",
	"Flog Symbols",
})
harfbuzz_features = { "calt=1", "clig=1", "liga=1" }
config.font_size = 14

-- Color Configuration
config.color_scheme = "Kanagawa (Gogh)"
config.force_reverse_video_cursor = true

-- Window Configuration
wezterm.on("gui-startup", function(cmd)
	local tab, pane, window = mux.spawn_window(cmd or {})
	window:gui_window():maximize()
end)
config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}
config.window_background_opacity = opacity
config.window_close_confirmation = "NeverPrompt"

-- Performance Settings
config.cursor_blink_rate = 0
config.front_end = "WebGpu"

-- Tab Bar Configuration
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.show_tab_index_in_tab_bar = false
config.use_fancy_tab_bar = false
-- config.colors.tab_bar = {}

-- Tab Formatting
wezterm.on("format-tab-title", function(tab, _, _, _, hover)
	local background = config.colors.brights[1]
	local foreground = config.colors.foreground

	if tab.is_active then
		background = config.colors.brights[7]
		foreground = config.colors.background
	elseif hover then
		background = config.colors.brights[8]
		foreground = config.colors.background
	end

	local title = tostring(tab.tab_index + 1)
	return {
		{ Foreground = { Color = background } },
		{ Text = " " },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = title },
		{ Foreground = { Color = background } },
		{ Text = " " },
	}
end)

-- Keybindings
-- config.keys = {}

return config
