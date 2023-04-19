#==========================================================================#
#========================= Import modules =================================#
#==========================================================================#
import os
import subprocess
from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
#==========================================================================#
#========================= Created variables  =============================#
#==========================================================================#
mod = "mod1"
myTerminal = guess_terminal()
myBrowser  = "qutebrowser"
myLauncher = "rofi -show drun -show-icons"
myPowerMenu= "bash /home/frhxm/.config/rofi/scripts/powerMenu.sh"
catppuccin = {
"Rosewater" : "#f5e0dc",
"Flamingo"  : "#f2cdcd",
"Pink"      : "#f5c2e7",
"Mauve"     : "#cba6f7",
"Red"       : "#f38ba8",
"Maroon"    : "#eba0ac",
"Peach"     : "#fab387",
"Yellow"    : "#f9e2af",
"Green"     : "#a6e3a1",
"Teal"      : "#94e2d5",
"sky"       : "#89dceb",
"Sapphire"  : "#74c7ec",
"Blue"      : "#89b4fa",
"Lavender"  : "#b4befe",
"Text"      : "#cdd6f4",
"Subtext1"  : "#bac2de",
"Subtext0"  : "#a6adc8",
"Overlay2"  : "#9399b2",
"Overlay1"  : "#7f849c",
"Overlay0"  : "#6c7086",
"Surface2"  : "#585b70",
"Surface1"  : "#45475a",
"Surface0"  : "#313244",
"Base"      : "#1e1e2e",
"Mantle"    : "#181825",
"Crust"     : "#11111b",
}
#==========================================================================#
#========================= Custom Keys  ===================================#
#==========================================================================#
keys = [
    #=-/ Main sys control /-=#
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "Shift"], "c", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc='toggle fullscreen'),
    Key([mod], "t", lazy.window.toggle_floating(), desc='toggle fullscreen'),
    #=-/ Switch between windows /-=#
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "Space", lazy.layout.next(), desc="Move window focus to other window"),
    #=-/ Move windows left@right or up@down /-=#
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    #=-/ Shrink  window /-=#
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "Return", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
    #=-/ Meltimedia /-=#
    Key(["Control"], "F9", lazy.spawn("amixer set Master 5%+"), desc='Volume up 5%'),
    Key(["Control"], "F8", lazy.spawn("amixer set Master 5%-"), desc='volume down 5%'),
    Key([], "XF86AudioMute", lazy.spawn("amixer set Master toggle"), desc='Volume Mute'),
    Key(["Control"], "F6", lazy.spawn("xbacklight -inc 30"), desc='brightness up'),
    Key(["Control"], "F5", lazy.spawn("xbacklight -dec 5"), desc='brightness down'),
    #=-/ Programs /-=#
    Key([mod, "Shift"], "Return", lazy.spawn(myTerminal), desc="Launch terminal"),
    Key([mod], "p", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "d", lazy.spawn(myLauncher), desc="Launch Rofi"),
    Key([mod], "w", lazy.spawn(myBrowser), desc="Launch qutebrowser"),
    Key([mod, "Shift"], "e", lazy.spawn(myPowerMenu), desc="Launch Power Menu"),
]
#==========================================================================#
#========================= Groups / Workspaces  ===========================#
#==========================================================================#
#groups = [Group(i) for i in "123456789"]
groups = [
    Group('1', label="一"),
    Group('2', label="二"),
    Group('3', label="三"),
    Group('4', label="四"),
    Group('5', label="五"),
    Group('6', label="六"),
    Group('7', label="七"),
    Group('8', label="八"),
    Group('9', label="九"),
]
for i in groups:
    keys.extend([Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Switch to group {}".format(i.name),),
                 Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True), desc="Switch to & move focused window to group {}".format(i.name)),])
#==========================================================================#
#========================= ScratchPad / DropDown ==========================#
#==========================================================================#
#=-/ Append scratchpad with dropdowns to groups /-=#
groups.append(ScratchPad('scratchpad', [
    DropDown('term', 'kitty', width=0.4, height=0.5, x=0.3, y=0.1, opacity=1),
    DropDown('vol', 'kitty pulsemixer', width=0.4, height=0.6, x=0.3, y=0.1, opacity=1),
    DropDown('browser', 'firefox', width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
]))
#=-/ extend keys list with keybinding for scratchpad /-=#
keys.extend([
    Key(["control"], "1", lazy.group['scratchpad'].dropdown_toggle('term')),
    Key(["control"], "2", lazy.group['scratchpad'].dropdown_toggle('vol')),
    Key(["control"], "3", lazy.group['scratchpad'].dropdown_toggle('browser')),
])
#==========================================================================#
#========================= Layouts Algorithms  ============================#
#==========================================================================#
layout_theme = {
                "margin": 2,
                "border_width": 3,
                "border_on_single": True,
                "border_focus" : catppuccin['Mauve'],
                "border_normal": catppuccin['Base'],
               }
layouts = [
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
    # layout.Stack(**layout_theme,num_stacks=2),
    # layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.MonadTall(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(**layout_theme),
    # layout.TreeTab(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
]
#==========================================================================#
#======================= Screens && widgets/bar ===========================#
#==========================================================================#
#=-/ Default settings for widgets /-=#
widget_defaults = dict(
    font="JetBrains Mono",
    fontsize=14,
    padding=3,
)
extension_defaults = widget_defaults.copy()

# █▄▄ ▄▀█ █▀█
# █▄█ █▀█ █▀▄
screens = [
    Screen(
        top=bar.Bar(
            [
                #=-/ Show Propmt Spawn After Click [Mod] "p" -=/#
                widget.Prompt(
                    background=catppuccin["Overlay2"],
                    foreground=catppuccin["Crust"],
                ),
                #=-/ Show Icons Distro /-=#
                widget.TextBox(
                    text="",
                    padding=10,
                    fontsize=30,
                    foreground=catppuccin["Blue"],
                    background=catppuccin["Crust"],
                ),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Surface2"],
                    foreground=catppuccin["Crust"],
                ),
                #=-/ Show Workspaces /-=#
                widget.GroupBox(
                    highlight_method="text",
                    this_current_screen_border=catppuccin["Green"],
                    urgent_alert_method="text",
                    urgent_text=catppuccin["Red"],
                    active=catppuccin["Text"],
                    inactive=catppuccin["Surface0"],
                    fontsize=15,
                    disable_drag=True,
                    background=catppuccin["Surface2"],
                ),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Mantle"],
                    foreground=catppuccin["Surface2"],
                ),
                #=-/ Show Current Layout Icons -=/#
                widget.CurrentLayoutIcon(
                    scale=0.65,
                    padding=10,
                    background=catppuccin["Mantle"],
                    foreground=catppuccin["Surface0"],
                ),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Surface0"],
                    foreground=catppuccin["Mantle"],
                ),
                #=-/ Show Window Name -=/#
                widget.WindowName(
                    background=catppuccin["Surface0"],
                    padding=10,
                ),
                #=-/ Show Number Of Update -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Surface0"],
                    foreground=catppuccin["Red"],
                ),
                widget.CheckUpdates(
                    background  = catppuccin['Red'],
                    foreground  = catppuccin["Red"],
                    font        ="Font Awesome 6 Free Solid",
                    fontsize    = 12,
                    padding     = 15,
                    distro      = 'Arch_checkupdates',
                    no_update_string =' ',
                    display_format   = '  {updates}',
                    mouse_callbacks={
                        "Button1": lazy.spawn("kitty sudo pacman -Syu"),
                    },
                ),
                #=-/ Show Battery -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Red"],
                    foreground=catppuccin["Pink"],
                ),
                widget.Battery(
                    format='{char} {percent:2.0%}',
                    charge_char='',
                    discharge_char='',
                    full_char='',
                    unknown_char= '🔌', #''
                    empty_char='',
                    show_short_text=False,
                    foreground=catppuccin['Surface0'],
                    background=catppuccin['Pink'],
                ),
                #=-/ Show Light -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Pink"],
                    foreground=catppuccin["Mauve"],
                ),
                widget.Backlight(
                    background=catppuccin["Mauve"],
                    foreground=catppuccin["Surface0"],
                    backlight_name="intel_backlight",
                    fmt=" {}",
                ),
                #=-/ Show Volume -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Mauve"],
                    foreground=catppuccin["Lavender"],
                ),
                widget.TextBox(
                    background=catppuccin["Lavender"],
                    foreground=catppuccin["Surface0"],
                    text=" ",
                    font="Font Awesome 6 Free Solid",
                    padding=0,
                ),
                widget.PulseVolume(
                    background=catppuccin["Lavender"],
                    foreground=catppuccin["Surface0"],
                    limit_max_volume="True",
                    update_interval=0.01,
                    mouse_callbacks={
                        "Button3": lazy.spawn("kitty pulsemixer"),
                    },
                ),
                #=-/ Show Clock -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Lavender"],
                    foreground=catppuccin["Sapphire"],
                ),
                widget.Clock(
                    format="%I:%M %p",
                    background=catppuccin["Sapphire"],
                    foreground=catppuccin["Mantle"],
                ),
                #=-/ Show System Tray -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Sapphire"],
                    foreground=catppuccin["Base"],
                ),
                widget.Systray(
                    background=catppuccin["Base"],
                ),
                #=-/ Show Date -=/#
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Base"],
                    foreground=catppuccin["Green"],
                ),
                widget.Clock(
                    format="%a %Y-%m-%d",
                    background=catppuccin["Green"],
                    foreground=catppuccin["Mantle"],
                ),
                #=-/ Show Button Shutdown Qtile -=#/
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=22,
                    background=catppuccin["Green"],
                    foreground=catppuccin["Mantle"],
                ),
                widget.TextBox(
                    text=" ",
                    background=catppuccin["Mantle"],
                    foreground=catppuccin["Red"],
                    fontsize=15,
                    mouse_callbacks={
                        "Button1": lazy.shutdown(),
                    },
                ),
            ],
            size=30,
            background=catppuccin["Base"],
            foreground=catppuccin["Text"],
            opacity=0.80,
            margin=[2, 5, 2, 5],
        ),
    ),
]
#==========================================================================#
#======================= Floating && rules ================================#
#==========================================================================#
#=-/ Drag floating layouts /-=#
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]
#=-/ Cursor rules /-=#
dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
#=-/ Window rules /-=#
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"), # gitk
        Match(wm_class="makebranch"),   # gitk
        Match(wm_class="maketag"),      # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),    # gitk
        Match(title="pinentry"),        # GPG key password entry
        #=-/ My Floating Window /-=#
        Match(wm_class="blueman-manager"),
        Match(wm_class="lxappearance"),
        Match(wm_class="sxiv"),
    ],
    **layout_theme,
)
#==========================================================================#
#============================ Others ======================================#
#==========================================================================#
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
wmname = "LG3D"
#==========================================================================#
#============================ Autostart ===================================#
#==========================================================================#
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/scripts/autostart.sh')
    subprocess.Popen([home])
