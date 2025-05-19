local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action
local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

config = wezterm.config_builder()

config = {
  automatically_reload_config = true,
  -- Keys
  disable_default_key_bindings = true,
  leader = { key = "a", mods = "CTRL", timeout_milliseconds = 2000 },
  -- Window
  window_close_confirmation = "NeverPrompt",
  window_decorations = "RESIZE",
  default_cursor_style = "BlinkingBlock",
  animation_fps = 1,
  color_scheme = "Kanagawa (Gogh)",
  font = wezterm.font_with_fallback {
    "CommitMonoVoid Nerd Font",
    "Flog Symbols"
  },
  harfbuzz_features = { "calt=1", "clig=1", "liga=1" },
  font_size = 9,
  use_fancy_tab_bar = false,
  show_new_tab_button_in_tab_bar = false,
  hide_tab_bar_if_only_one_tab = true,
  tab_max_width = 50,
}

config.default_prog = { "C:\\Program Files\\PowerShell\\7\\pwsh.exe" }

config.wsl_domains = {
  {
    name = "WSL:FedoraLinux-42",
    distribution = "FedoraLinux-42",
    username = "paulinux",
    default_cwd = "~",
  },
}

config.default_domain = "WSL:FedoraLinux-42"

-- config.colors = {}

-- config.tab_bar_style = {}

config.keys = {
  { key = "l",     mods = "LEADER",                    action = act.ShowLauncher },
  { key = "n",     mods = "LEADER",                    action = act.SpawnWindow },
  { key = "f",     mods = "LEADER",                    action = act.ToggleFullScreen },
  { key = "c",     mods = "LEADER|CTRL",               action = act.CopyTo("Clipboard") },
  { key = "v",     mods = "CTRL",                      action = act.PasteFrom("Clipboard") },
  { key = "Copy",  action = act.CopyTo("Clipboard") },
  { key = "Paste", action = act.PasteFrom("Clipboard") },
  { key = "w",     mods = "LEADER",                    action = act.CloseCurrentTab({ confirm = false }) },
  { key = "t",     mods = "LEADER",                    action = act.SpawnTab("CurrentPaneDomain") },
  { key = "T",     mods = "LEADER",                    action = act.SpawnTab("DefaultDomain") },
  { key = "Tab",   mods = "CTRL",                      action = act.ActivateTabRelative(1) },
  { key = "P",     mods = "LEADER",                    action = act.ActivateCommandPalette },
  { key = "-",     mods = "CTRL",                      action = act.DecreaseFontSize },
  { key = "=",     mods = "CTRL",                      action = act.IncreaseFontSize },
  { key = "0",     mods = "CTRL",                      action = act.ResetFontSize },
}

config.window_padding = {
  left = "0cell",
  right = "0cell",
  top = "0cell",
  bottom = "0cell",
}

tabline.setup({
  options = {
    icons_enabled = true,
    theme = config.color_scheme,
    tabs_enabled = true,
    theme_overrides = {},
    section_separators = {
      left = wezterm.nerdfonts.pl_left_hard_divider,
      right = wezterm.nerdfonts.pl_right_hard_divider,
    },
    component_separators = {
      left = wezterm.nerdfonts.pl_left_soft_divider,
      right = wezterm.nerdfonts.pl_right_soft_divider,
    },
    tab_separators = {
      left = wezterm.nerdfonts.pl_left_hard_divider,
      right = wezterm.nerdfonts.pl_right_hard_divider,
    },
  },
  sections = {
    tabline_a = { "mode" },
    tabline_b = { "workspace" },
    tabline_c = { "hostname" },
    tab_active = {
      "index",
      { "parent", padding = 0 },
      ":",
      { "cwd",    padding = { left = 0, right = 1 } },
      { "zoomed", padding = 0 },
    },
    tab_inactive = { "index", { "process", padding = { left = 0, right = 1 } } },
    tabline_x = { "ram", "cpu" },
    tabline_y = { "datetime", "battery" },
    tabline_z = { "domain" },
  },
  extensions = {},
})

return config
