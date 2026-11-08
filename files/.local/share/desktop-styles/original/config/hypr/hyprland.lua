-- Hyprland Lua config (migrated from hyprland.conf)
-- Since Hyprland 0.55, hyprlang (.conf) is deprecated in favor of Lua.
-- Docs: https://wiki.hypr.land/Configuring/Start/

------------------
---- MONITORS ----
------------------

hl.monitor({ output = "DP-3",     mode = "1920x1080@240", position = "0x0",    scale = 1 })
hl.monitor({ output = "HDMI-A-1", mode = "2560x1440@120", position = "1920x0", scale = 1 })


-----------------------
---- WORKSPACE RULES --
-----------------------

hl.workspace_rule({ workspace = "1", monitor = "DP-3", default = true, persistent = true })
for i = 2, 10 do
    hl.workspace_rule({ workspace = tostring(i), monitor = "DP-3" })
end
for i = 11, 20 do
    hl.workspace_rule({ workspace = tostring(i), monitor = "HDMI-A-1" })
end


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("~/.scripts/startup")
end)

-- Alacritty vertical padding: min(1% of window height, 14px).
local terminalPadding = {}
hl.timer(function()
    for _, window in ipairs(hl.get_windows({ class = "Alacritty" })) do
        local padding = math.min(14, math.floor(window.size.y * 0.01 + 0.5))
        if terminalPadding[window.pid] ~= padding then
            local socket = os.getenv("XDG_RUNTIME_DIR") .. "/Alacritty-*-" .. window.pid .. ".sock"
            hl.exec_cmd("alacritty msg --socket " .. socket .. " config --window-id -1 window.padding.y=" .. padding)
            terminalPadding[window.pid] = padding
        end
    end
end, { timeout = 250, type = "repeat" })


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

hl.env("XCURSOR_SIZE", "24")
hl.env("BROWSER", "helium-browser")


-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    cursor = {
        default_monitor = "DP-3",
    },

    input = {
        kb_layout  = "us",
        kb_variant = "",
        kb_model   = "",
        kb_options = "",
        kb_rules   = "",

        repeat_rate  = 30,
        repeat_delay = 250,

        follow_mouse = 1,

        touchpad = {
            natural_scroll = false,
        },

        sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.
    },

    general = {
        gaps_in     = 3,
        gaps_out    = 15,
        border_size = 2,

        col = {
            active_border   = { colors = {"rgba(9cdedb99)", "rgba(b5c8ed66)"}, angle = 45 },
            inactive_border = "rgba(78909c33)",
        },

        layout = "dwindle",
    },

    decoration = {
        rounding = 4,

        blur = {
            enabled           = true,
            size              = 8,
            passes            = 3,
            new_optimizations = true,
            ignore_opacity    = true,
            xray              = false,
        },

        shadow = {
            enabled = true,
            range = 22,
            render_power = 3,
            color = "rgba(050c1840)",
        },

        inactive_opacity = 0.95,
        active_opacity   = 1.0,
    },

    animations = {
        enabled = true,
    },

    dwindle = {
        preserve_split = true,
    },

    misc = {
        disable_hyprland_logo = true,
    },
})


--------------------
---- ANIMATIONS ----
--------------------

hl.curve("md3_standard",  { type = "bezier", points = { {0.2, 0},   {0, 1}      } })
hl.curve("md3_decel",     { type = "bezier", points = { {0.05, 0.7},{0.1, 1}    } })
hl.curve("md3_accel",     { type = "bezier", points = { {0.3, 0},   {0.8, 0.15} } })
hl.curve("overshot",      { type = "bezier", points = { {0.05, 0.9},{0.1, 1.1}  } })
hl.curve("crazyshot",     { type = "bezier", points = { {0.1, 1.5}, {0.76, 0.92}} })
hl.curve("hyprnostretch", { type = "bezier", points = { {0.05, 0.9},{0.1, 1.0}  } })
hl.curve("fluent_decel",  { type = "bezier", points = { {0.1, 1},   {0, 1}      } })
hl.curve("easeInOutCirc", { type = "bezier", points = { {0.05, 0},  {0.15, 1}   } })
hl.curve("easeOutCirc",   { type = "bezier", points = { {0, 0.55},  {0.45, 1}   } })

hl.animation({ leaf = "windows",          enabled = true, speed = 4, bezier = "md3_decel", style = "popin 96%" })
hl.animation({ leaf = "border",           enabled = true, speed = 3, bezier = "md3_standard" })
hl.animation({ leaf = "fade",             enabled = true, speed = 3, bezier = "md3_standard" })
hl.animation({ leaf = "workspaces",       enabled = true, speed = 3, bezier = "md3_standard", style = "fade" })
hl.animation({ leaf = "specialWorkspace", enabled = true, speed = 3, bezier = "md3_decel", style = "fade" })


-- Blur the visible shell panels without blurring their transparent margins.
hl.layer_rule({
    name = "shell_glass",
    match = { namespace = "quickshell" },
    blur = true,
    ignore_alpha = 0.05,
})


----------------------
---- WINDOW RULES ----
----------------------

-- Helium
hl.window_rule({
    name        = "helium",
    match       = { class = "helium" },
    opacity     = "0.88 override 0.82 override 1.0 override",
    no_auto_hdr = true,
})

-- Discord
hl.window_rule({
    name  = "discord_float",
    match = { class = "discord" },
    float = true,
    size  = {"monitor_w*0.90", "monitor_h*0.90"},
    move  = {"monitor_w*0.05", "monitor_h*0.06"},
})

-- Slack
hl.window_rule({
    name  = "slack_float",
    match = { class = "Slack" },
    float = true,
    size  = {"monitor_w*0.90", "monitor_h*0.90"},
    move  = {"monitor_w*0.05", "monitor_h*0.06"},
})

-- Telegram
hl.window_rule({
    name      = "telegram_float",
    match     = { class = "org.telegram.desktop" },
    workspace = "special:telegram",
    float     = true,
    size      = {"monitor_w*0.30", "monitor_h*0.85"},
    move      = {"monitor_w*0.35", "monitor_h*0.06"},
})

-- ranger
hl.window_rule({
    name  = "ranger_float",
    match = { title = "ranger" },
    float = true,
    size  = {"monitor_w*0.60", "monitor_h*0.60"},
    move  = {"monitor_w*0.20", "monitor_h*0.18"},
})

-- Emacs -> main monitor (workspace 1 lives on DP-3)
hl.window_rule({
    name      = "emacs_main_monitor",
    match     = { class = "Emacs" },
    workspace = 1,
})


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER"
local panelIpc = "quickshell ipc -p ~/.config/quickshell/shell.qml call panels "

---- Essential Bindings
hl.bind(mainMod .. " + RETURN",         hl.dsp.exec_cmd("alacritty"))
hl.bind(mainMod .. " + S",              hl.dsp.exec_cmd("slack"))
hl.bind(mainMod .. " + SHIFT + RETURN", hl.dsp.exec_cmd("~/.config/startpage/open"))
hl.bind(mainMod .. " + Q",              hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Q",      hl.dsp.exit())
hl.bind(mainMod .. " + D",              hl.dsp.exec_cmd(panelIpc .. "launcher"))
hl.bind(mainMod .. " + SHIFT + W",      hl.dsp.exec_cmd("~/.config/quickshell/scripts/dashboard"))

---- Resolution Bindings
hl.bind(mainMod .. " + CTRL + SHIFT + 1", hl.dsp.exec_cmd("hyprctl keyword monitor DP-3,1920x1080@240,0x0,1"))
hl.bind(mainMod .. " + CTRL + SHIFT + 2", hl.dsp.exec_cmd("hyprctl keyword monitor DP-3,1920x1080@240,0x0,1,transform,1"))
hl.bind(mainMod .. " + CTRL + SHIFT + R", hl.dsp.exec_cmd("~/.scripts/rotatescreen"))

hl.bind("F1", hl.dsp.exec_cmd("~/.config/hypr/setbrightness 2 -10"))
hl.bind("F2", hl.dsp.exec_cmd("~/.config/hypr/setbrightness 2 +10"))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("~/.config/hypr/setbrightness 1 -10"))
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("~/.config/hypr/setbrightness 1 +10"))

---- Media Bindings
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("pactl set-sink-volume @DEFAULT_SINK@ +5%"))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("pactl set-sink-volume @DEFAULT_SINK@ -5%"))
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("pactl set-sink-mute @DEFAULT_SINK@ toggle"))

---- Sonos Speaker Bindings (plain F10/F11/F12)
hl.bind("F12", hl.dsp.exec_cmd("/home/prayuj/.nix-profile/bin/python3 /home/prayuj/.config/quickshell/scripts/sonos-vol up"),   { repeating = true })
hl.bind("F11", hl.dsp.exec_cmd("/home/prayuj/.nix-profile/bin/python3 /home/prayuj/.config/quickshell/scripts/sonos-vol down"), { repeating = true })
hl.bind("F10", hl.dsp.exec_cmd("/home/prayuj/.nix-profile/bin/python3 /home/prayuj/.config/quickshell/scripts/sonos-vol mute"))
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"))
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"))
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"))

---- Screenshot Bindings
hl.bind(mainMod .. " + CTRL + SHIFT + 3", hl.dsp.exec_cmd("HYPRSHOT_DIR='/home/prayuj/Pictures/screenshots' hyprshot -m output"))
hl.bind(mainMod .. " + CTRL + SHIFT + 4", hl.dsp.exec_cmd("HYPRSHOT_DIR='/home/prayuj/Pictures/screenshots' hyprshot -m region"))
hl.bind(mainMod .. " + CTRL + SHIFT + 5", hl.dsp.exec_cmd("HYPRSHOT_DIR='/home/prayuj/Pictures/screenshots' hyprshot -m window"))
hl.bind(mainMod .. " + CTRL + SHIFT + 6", hl.dsp.exec_cmd("hyprcap rec -n -o '/home/prayuj/Pictures/recordings'"))

---- App Toggle Bindings
hl.bind(mainMod .. " + T",         hl.dsp.exec_cmd(panelIpc .. "telegram"))
hl.bind(mainMod .. " + SHIFT + T", hl.dsp.exec_cmd(panelIpc .. "telegramFullscreen"))
hl.bind(mainMod .. " + R",         hl.dsp.exec_cmd(panelIpc .. "telegram"))
hl.bind(mainMod .. " + SHIFT + N", hl.dsp.exec_cmd(panelIpc .. "news"))
-- hl.bind(mainMod .. " + SHIFT + P", hl.dsp.exec_cmd("echo toggle > /tmp/qs-portfolio-toggle")) -- portfolio widget temporarily hidden
hl.bind(mainMod .. " + SHIFT + SPACE", hl.dsp.exec_cmd(panelIpc .. "media"))
hl.bind(mainMod .. " + SHIFT + I",     hl.dsp.exec_cmd(panelIpc .. "notifications"))

---- Window Bindings
hl.bind(mainMod .. " + SPACE", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + F",     hl.dsp.window.fullscreen({ action = "toggle" }))

hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "down" }))

hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "left" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "right" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "up" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "down" }))

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

---- Workspace Bindings
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key, hl.dsp.exec_cmd("~/.config/hypr/workspace focus " .. i))
end

hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

---- Window + Workspace Bindings
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.exec_cmd("~/.config/hypr/workspace move " .. i))
end

---- Alt-Tab Behavior
hl.bind(mainMod .. " + Tab", function()
    hl.dispatch(hl.dsp.window.cycle_next({ next = false }))
    hl.dispatch(hl.dsp.window.alter_zorder({ mode = "top" }))
end)

hl.bind("ALT + Tab", function()
    hl.dispatch(hl.dsp.window.cycle_next({ next = false }))
    hl.dispatch(hl.dsp.window.alter_zorder({ mode = "top" }))
end)

-- Shared dictation shortcut, preserved across desktop styles.
dofile(os.getenv("HOME") .. "/.config/hypr/voxtype.lua")
