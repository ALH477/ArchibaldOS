{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    pcmanfm vim brave polybar feh kitty wireplumber cava playerctl
    scrot
    dmenu
    jetbrains-mono
  ];

  dwmConf = ''
    /* See LICENSE file for copyright and license details. */
    #include <X11/XF86keysym.h>

    /* appearance */
    static const unsigned int borderpx  = 3;        /* border pixel of windows */
    static const unsigned int gappx     = 15;       /* gap pixel between windows */
    static const unsigned int snap      = 32;       /* snap pixel */
    static const int showbar            = 1;        /* 0 means no bar */
    static const int topbar             = 1;        /* 0 means bottom bar */
    static const char *fonts[]          = { "JetBrains Mono:size=11:antialias=true:autohint=true", "Noto Color Emoji:size=10" };
    static const char dmenufont[]       = "JetBrains Mono:size=11:antialias=true:autohint=true";
    static const char col_gray1[]       = "#222222";
    static const char col_gray2[]       = "#444444";
    static const char col_gray3[]       = "#bbbbbb";
    static const char col_gray4[]       = "#eeeeee";
    static const char col_gray5[]       = "#282828encodeURIComponent(document.location.href);
    static const char col_gray6[]       = "#616673";
    static const char col_cyan[]        = "#005577";
    static const char col_aqua[]        = "#427b58";
    static const char col_blue[]        = "#458588";
    static const char col_purple[]      = "#EE82EE";
    static const char col_black[]       = "#000000";
    static const char col_red[]         = "#9e170e";
    static const char col_yellow[]      = "#ffff00";
    static const char col_pink[]        = "#934b52";
    static const char col_white[]       = "#ffffff";
    static const char col_green[]       = "#77a347";
    static const char col_border[]      = "#cc241d";
    static const char *colors[][3]      = {
      /*               fg         bg         border   */
      /*               text       bg         border   */
      [SchemeNorm] = { col_gray3, col_gray1, col_black },
      [SchemeSel]  = { col_gray4, col_pink,  col_border },
      [SchemeRed]  = { col_red,   col_gray1, col_black },
      [SchemeGreen]= { col_green, col_gray1, col_black }, 
      [SchemeBlue] = { col_blue,  col_gray1, col_black }, 
    };

    static const unsigned int alphas[][3]      = {
      /*                fg      bg    border     */
      [SchemeNorm]  = { OPAQUE, 0xD0, OPAQUE },
      [SchemeSel]   = { OPAQUE, 0xD0, OPAQUE },
      [SchemeRed]   = { OPAQUE, 0xD0, OPAQUE },
      [SchemeGreen] = { OPAQUE, 0xD0, OPAQUE },
      [SchemeBlue] = { OPAQUE, 0xD0, OPAQUE },
    };

    /* tagging */
    static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

    static const Rule rules[] = {
      /* xprop(1):
       *	WM_CLASS(STRING) = instance, class
       *	WM_NAME(STRING) = title
       */
      /* class      instance    title       tags mask     iscentered     isfloating   monitor */
      { "Gimp",     NULL,       NULL,       0,            0,             1,           -1 },
      { "Brave-browser",  NULL,       NULL,       1 << 1,       0,           0,           -1 },
      { "Spotify",  NULL,       NULL,       1 << 8,       0,             0,           -1 },
    };

    /* layout(s) */
    static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
    static const int nmaster     = 1;    /* number of clients in master area */
    static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
    static const int lockfullscreen = 1;

    static const Layout layouts[] = {
      /* symbol     arrange function */
      { "[]=",      gaps },    /* first entry is default */
      { "><>",      NULL },    /* no layout function means floating behavior */
      { "[M]",      monocle },
      { "|M|",      centeredmaster },
      { ">M>",      centeredfloatingmaster }, 
      { "|||",      col },
      { "[G]=",     tile },
    };

    /* key definitions */
    #define MODKEY Mod1Mask
    #define SUPER Mod4Mask
    #define TAGKEYS(KEY,TAG) \
      { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
      { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

    /* helper for spawning shell commands in the pre dwm-5.0 fashion */
    #define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

    /* commands */
    static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
    static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf", col_gray4, NULL };
    static const char *termcmd[]  = { "kitty", NULL };
    static const char *browsercmd[] = { "brave", NULL};
    static const char *spotifycmd[] = {"spotify", NULL};
    static const char *volumeup[]  = { "wpctl", "set-volume", "-l", "1", "@DEFAULT_AUDIO_SINK@", "5%+", NULL };
    static const char *volumedown[]  = { "wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%-", NULL };
    static const char *mutevol[] = { "wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle", NULL };
    static const char *screenshot[] = { "scrot", "/home/nixos/Pictures/Screenshots/screenshot-%F-%T.png", NULL };
    static const char *hydramesh[] = { "hydramesh-toggle", NULL };
    static const char *cheatsheet[] = { "/home/nixos/.config/hypr/keybindings_cheatsheet.sh", NULL };
    static const char *bluetooth[] = { "blueman-applet", "&", "blueman-manager", NULL };
    static const char *brightnessup[] = { "xbacklight", "-inc", "10", NULL };
    static const char *brightnessdown[] = { "xbacklight", "-dec", "10", NULL };
    static const char *bluetoothmenu[] = { "kitty", "-e", "bluetoothctl", NULL };
    static const char *shutdown[] = { "sudo", "shutdown", "-P", "now", NULL };
    static const char *mount[] = { "sudo", "mount", "/dev/sdc1", "/media/usb", NULL};
    static const char *umount[] = { "sudo", "umount", "/media/usb", NULL};
    static const char *files[] = { "pcmanfm", NULL };
    static const char *audio[] = { "/home/nixos/scripts/audio.sh", NULL };
    static const char *reboot[] = { "sudo", "reboot", NULL };
    static const char *anki[] = { "anki", NULL };
    static const char *ranger[] = { "kitty", "-e", "ranger", NULL };
    static const char *zoomConference[] = { "zoom", NULL };
    static const char *weather[] = { "kitty", "-e", "/home/nixos/scripts/weather.sh", NULL };
    static const char *python[] = { "kitty", "-e", "python", NULL };
    static const char *newsboat[] = { "kitty", "-e", "newsboat", NULL };
    static const char *kanji[] = { "/home/nixos/scripts/rtk_keyboard.sh", NULL };
    static const char *record[] = { "kitty", "-e", "/home/nixos/scripts/record_desktop.sh", NULL };
    static const char *engOCR[] = { "/home/nixos/scripts/ocr_eng.sh", NULL };
    static const char *jpnOCR[] = { "/home/nixos/scripts/ocr_jpn.sh", NULL };
    static const char *markdownImg[] = { "/home/nixos/scripts/markdown_img.sh", NULL };

    static const Key keys[] = {
      /* modifier                     key                      function        argument */
      { MODKEY,                       XK_p,                    spawn,          {.v = dmenucmd } },
      { MODKEY|ShiftMask,             XK_Return,               spawn,          {.v = termcmd } },
      { MODKEY|ShiftMask,             XK_b,                    spawn,          {.v = browsercmd } },
      { MODKEY,                       XK_s,                    spawn,          {.v = spotifycmd} },
      { 0,                            XF86XK_AudioRaiseVolume, spawn,          {.v = volumeup } },
      { 0,                            XF86XK_AudioLowerVolume, spawn,          {.v = volumedown } },
      { 0,                            XF86XK_AudioMute,        spawn,          {.v = mutevol } },
      { 0,                            XK_Print,                spawn,          {.v = screenshot } },
      { MODKEY|ShiftMask,             XK_h,                    spawn,          {.v = hydramesh } },
      { MODKEY|ShiftMask,             XK_k,                    spawn,          {.v = cheatsheet } },
      { 0,                            XF86XK_MonBrightnessUp,  spawn,          {.v = brightnessup} },
      { 0,                            XF86XK_MonBrightnessDown,spawn,          {.v = brightnessdown} },
      { MODKEY|ControlMask|ShiftMask, XK_q,                    quit,           {1} }, 
      { ControlMask|ShiftMask,        XK_H,                    setcfact,       {.f = +0.25} },
      { ControlMask|ShiftMask,        XK_L,                    setcfact,       {.f = -0.25} },
      { MODKEY|ShiftMask,             XK_o,                    setcfact,       {.f =  0.00} }, 
      { MODKEY,                       XK_u,                    setlayout,      {.v = &layouts[3]} },
      { MODKEY,                       XK_o,                    setlayout,      {.v = &layouts[4]} }, 
      { MODKEY|ShiftMask,             XK_v,                    setlayout,      {.v = &layouts[5]} },
      { MODKEY|ControlMask,           XK_k,                    spawn,          SHCMD("setxkbmap -option caps:swapescape") }, 
      { MODKEY|ControlMask,           XK_b,                    spawn,          {.v = bluetoothmenu } },
      { MODKEY|ControlMask|ShiftMask, XK_p,                    spawn,          {.v = shutdown } },
      { MODKEY|ControlMask,           XK_m,                    spawn,          {.v = mount } },
      { MODKEY|ControlMask|ShiftMask, XK_m,                    spawn,          {.v = umount} },
      { MODKEY,                       XK_e,                    spawn,          {.v = files} },
      { MODKEY,                       XK_r,                    spawn,          {.v = ranger} },
      { MODKEY,                       XK_a,                    spawn,          {.v = audio} },
      { MODKEY|ControlMask|ShiftMask, XK_r,                    spawn,          {.v = reboot} },
      { MODKEY|ShiftMask,             XK_a,                    spawn,          {.v = anki} },
      { MODKEY,                       XK_w,                    spawn,          {.v = weather} },
      { MODKEY,                       XK_z,                    spawn,          {.v = zoomConference} },
      { MODKEY|ShiftMask,             XK_p,                    spawn,          {.v = python} },
      { MODKEY,                       XK_n,                    spawn,          {.v = markdownImg} },
      { MODKEY|ShiftMask,             XK_n,                    spawn,          {.v = kanji} },
      { MODKEY|ShiftMask,             XK_r,                    spawn,          {.v = record} },
      { SUPER,                        XK_o,                    spawn,          {.v = engOCR} },
      { SUPER|ShiftMask,              XK_o,                    spawn,          {.v = jpnOCR} },
      { MODKEY,                       XK_b,                    togglebar,      {0} },
      { MODKEY,                       XK_k,                    focusstack,     {.i = +1 } },
      { MODKEY,                       XK_j,                    focusstack,     {.i = -1 } },
      { MODKEY,                       XK_i,                    incnmaster,     {.i = +1 } },
      { MODKEY,                       XK_d,                    incnmaster,     {.i = -1 } },
      { MODKEY,                       XK_h,                    setmfact,       {.f = -0.05} },
      { MODKEY,                       XK_l,                    setmfact,       {.f = +0.05} },
      { MODKEY,                       XK_Return,               zoom,           {0} },
      { MODKEY,                       XK_Tab,                  view,           {0} },
      { MODKEY|ShiftMask,             XK_c,                    killclient,     {0} },
      { MODKEY,                       XK_t,                    setlayout,      {.v = &layouts[0]} },
      { MODKEY,                       XK_f,                    setlayout,      {.v = &layouts[1]} },
      { MODKEY,                       XK_m,                    setlayout,      {.v = &layouts[2]} },
      { MODKEY,                       XK_g,                    setlayout,      {.v = &layouts[6]} },
      { MODKEY,                       XK_0,                    view,           {.ui = ~0 } },
      { MODKEY|ShiftMask,             XK_0,                    tag,            {.ui = ~0 } },
      { MODKEY|ShiftMask,             XK_h,                    focusmon,       {.i = -1 } },
      { MODKEY|ShiftMask,             XK_l,                    focusmon,       {.i = +1 } },
      { MODKEY|ShiftMask,             XK_comma,                tagmon,         {.i = -1 } },
      { MODKEY|ShiftMask,             XK_period,               tagmon,         {.i = +1 } },
      TAGKEYS(                        XK_1,                                    0)
      TAGKEYS(                        XK_2,                                    1)
      TAGKEYS(                        XK_3,                                    2)
      TAGKEYS(                        XK_4,                                    3)
      TAGKEYS(                        XK_5,                                    4)
      TAGKEYS(                        XK_6,                                    5)
      TAGKEYS(                        XK_7,                                    6)
      TAGKEYS(                        XK_8,                                    7)
      TAGKEYS(                        XK_9,                                    8)
      { MODKEY|ShiftMask,             XK_q,                    quit,           {0} },
    };

    /* button definitions */
    /* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
    static const Button buttons[] = {
      /* click                event mask      button          function        argument */
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
