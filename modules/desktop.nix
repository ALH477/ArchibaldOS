{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    pcmanfm vim brave polybar feh kitty wireplumber cava playerctl
    scrot
    dmenu
    jetbrains-mono
    noto-fonts-emoji
  ];

  dwmConf = ''
    #include <X11/XF86keysym.h>

    static const unsigned int borderpx  = 3;
    static const unsigned int gappx     = 6;
    static const unsigned int snap      = 32;
    static const int showbar            = 1;
    static const int topbar             = 1;
    static const char *fonts[]          = { "JetBrains Mono:size=11:antialias=true:autohint=true", "Noto Color Emoji:size=10" };
    static const char dmenufont[]       = "JetBrains Mono:size=11:antialias=true:autohint=true";
    static const char col_nord0[]       = "#2E3440";
    static const char col_nord3[]       = "#4C566A";
    static const char col_nord4[]       = "#D8DEE9";
    static const char col_nord7[]       = "#8FBCBB";
    static const char col_nord8[]       = "#88C0D0";
    static const char *colors[][3]      = {
      [SchemeNorm] = { col_nord4, col_nord0, col_nord3 },
      [SchemeSel]  = { col_nord0, col_nord7, col_nord8 },
    };

    static const char *tags[] = { "", "", "", "", "" };

    static const Rule rules[] = {
      { "Gimp",     NULL,       NULL,       0,            1,           -1 },
      { "Brave-browser",  NULL,       NULL,       1 << 1,       0,           -1 },
    };

    static const float mfact     = 0.55;
    static const int nmaster     = 1;
    static const int resizehints = 1;
    static const int lockfullscreen = 1;

    static const Layout layouts[] = {
      { "󰛊 ",      tile },
      { "󰉧 ",      NULL },
      { "󰍉 ",      monocle },
    };

    #define MODKEY Mod4Mask
    #define TAGKEYS(KEY,TAG) \
      { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
      { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

    #define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

    static char dmenumon[2] = "0";
    static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_nord0, "-nf", col_nord4, "-sb", col_nord7, "-sf", col_nord0, NULL };
    static const char *termcmd[]  = { "kitty", NULL };
    static const char *upvol[] = { "wpctl", "set-volume", "-l", "1", "@DEFAULT_AUDIO_SINK@", "5%+", NULL };
    static const char *downvol[] = { "wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%-", NULL };
    static const char *mutevol[] = { "wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle", NULL };
    static const char *screenshot[] = { "scrot", "/home/nixos/Pictures/Screenshots/screenshot-%F-%T.png", NULL };
    static const char *hydramesh[] = { "hydramesh-toggle", NULL };
    static const char *cheatsheet[] = { "/home/nixos/.config/hypr/keybindings_cheatsheet.sh", NULL };

    static const Key keys[] = {
      { MODKEY,                       XK_d,      spawn,          {.v = dmenucmd } },
      { MODKEY,                       XK_q,      spawn,          {.v = termcmd } },
      { MODKEY,                       XK_c,      killclient,     {0} },
      { MODKEY,                       XK_m,      quit,           {0} },
      { MODKEY,                       XK_h,      focusstack,     {.i = -1 } },
      { MODKEY,                       XK_l,      focusstack,     {.i = +1 } },
      { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
      { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
      { MODKEY,                       XK_1,      view,           {.ui = 1 << 0 } },
      { MODKEY,                       XK_2,      view,           {.ui = 1 << 1 } },
      { MODKEY,                       XK_3,      view,           {.ui = 1 << 2 } },
      { MODKEY,                       XK_4,      view,           {.ui = 1 << 3 } },
      { MODKEY,                       XK_5,      view,           {.ui = 1 << 4 } },
      { 0,                            XF86XK_AudioRaiseVolume, spawn, {.v = upvol } },
      { 0,                            XF86XK_AudioLowerVolume, spawn, {.v = downvol } },
      { 0,                            XF86XK_AudioMute, spawn, {.v = mutevol } },
      { MODKEY,                       XK_Print,  spawn,          {.v = screenshot } },
      { MODKEY|ShiftMask,             XK_h,      spawn,          {.v = hydramesh } },
      { MODKEY|ShiftMask,             XK_k,      spawn,          {.v = cheatsheet } },
      { MODKEY,                       XK_b,      togglebar,      {0} },
      { MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
      { MODKEY,                       XK_p,      incnmaster,     {.i = -1 } },
      { MODKEY,                       XK_Return, zoom,           {0} },
      { MODKEY,                       XK_Tab,    view,           {0} },
      { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
      { MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
      { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
      { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
      { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
      { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
      { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
      { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
      { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    };

    static const Button buttons[] = {
      { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
      { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
      { ClkWinTitle,          0,              Button2,        zoom,           {0} },
      { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
      { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
      { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
      { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
      { ClkTagBar,            0,              Button1,        view,           {0} },
      { ClkTagBar,            0,              Button3,        toggleview,     {0} },
      { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
      { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
    };
  '';

  customDwmPkg = pkgs.dwm.override { conf = dwmConf; };

  customDwm = pkgs.writeShellScriptBin "dwm" ''
    ${pkgs.polybar}/bin/polybar --config=/etc/polybar/config.ini &
    ${pkgs.feh}/bin/feh --bg-fill /etc/hypr/wallpaper.jpg &
    exec ${customDwmPkg}/bin/dwm
  '';
in {
  services.xserver.enable = true;
  services.xserver.windowManager.dwm.enable = true;
  services.xserver.windowManager.dwm.package = customDwm;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = false;
  services.displayManager.defaultSession = "none+dwm";
  services.displayManager.sddm.settings = {
    General.Background = "/etc/hypr/wallpaper.jpg";
  };

  environment.etc."polybar/config.ini".text = ''
    [bar/main]
    monitor =
    width = 100%
    height = 30
    radius = 0
    fixed-center = true

    background = #000000
    foreground = #ffffff

    line-size = 2
    line-color = #f00

    border-size = 0
    border-color = #00000000

    padding-left = 0
    padding-right = 2

    module-margin-left = 1
    module-margin-right = 2

    font-0 = fixed:pixelsize=10;1

    modules-left = cava
    modules-center = date
    modules-right = cpu memory

    tray-position = right
    tray-padding = 2

    wm-restack = generic

    [module/cava]
    type = custom/script
    tail = true
    exec = /etc/polybar/cava.sh
    format = <label>
    label = %output%

    [module/date]
    type = internal/date
    interval = 5
    date = %Y-%m-%d%
    time = %H:%M:%S
    label = %date% %time%

    [module/cpu]
    type = internal/cpu
    interval = 2
    format-prefix = "CPU "
    label = %percentage:2%%

    [module/memory]
    type = internal/memory
    interval = 2
    format-prefix = "RAM "
    label = %percentage_used%%
  '';

  environment.etc."polybar/cava.sh" = {
    source = ./cava.sh;
    mode = "0755";
  };

  environment.etc."live-audio-test.sh" = {
    source = ./live-audio-test.sh;
    mode = "0755";
  };

  environment.systemPackages = basicPackages;
}
