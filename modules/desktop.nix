{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    vim polybar feh kitty wireplumber cava playerctl
    dmenu
    jetbrains-mono
    noto-fonts-emoji
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
    static const char col_pink[]        = "#934b52";
    static const char col_black[]       = "#000000";
    static const char col_border[]      = "#cc241d";
    static const char *colors[][3]      = {
      /*               fg         bg         border   */
      [SchemeNorm] = { col_gray3, col_gray1, col_black },
      [SchemeSel]  = { col_gray4, col_pink,  col_border },
    };

    /* tagging */
    static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

    static const Rule rules[] = {
      /* xprop(1):
       *	WM_CLASS(STRING) = instance, class
       *	WM_NAME(STRING) = title
       */
      /* class      instance    title       tags mask     isfloating   monitor */
      { "Gimp",     NULL,       NULL,       0,            1,           -1 },
      { "Brave-browser",  NULL,       NULL,       1 << 1,       0,           -1 },
      { "Spotify",  NULL,       NULL,       1 << 8,       0,           -1 },
    };

    /* layout(s) */
    static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
    static const int nmaster     = 1;    /* number of clients in master area */
    static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
    static const int lockfullscreen = 1;

    static const Layout layouts[] = {
      /* symbol     arrange function */
      { "[]=",      tile },    /* first entry is default */
      { "><>",      NULL },    /* no layout function means floating behavior */
      { "[M]",      monocle },
    };

    /* key definitions */
    #define MODKEY Mod1Mask
    #define TAGKEYS(KEY,TAG) \
      { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
      { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
      { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

    /* commands */
    static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
    static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_pink, "-sf", col_gray4, NULL };
    static const char *termcmd[]  = { "kitty", NULL };

    static const Key keys[] = {
      /* modifier                     key                      function        argument */
      { MODKEY,                       XK_d,                    spawn,          {.v = dmenucmd } },
      { MODKEY,                       XK_q,                    spawn,          {.v = termcmd } },
      { MODKEY,                       XK_b,                    togglebar,      {0} },
      { MODKEY,                       XK_k,                    focusstack,     {.i = +1 } },
      { MODKEY,                       XK_j,                    focusstack,     {.i = -1 } },
      { MODKEY,                       XK_i,                    incnmaster,     {.i = +1 } },
      { MODKEY,                       XK_p,                    incnmaster,     {.i = -1 } },
      { MODKEY,                       XK_h,                    setmfact,       {.f = -0.05} },
      { MODKEY,                       XK_l,                    setmfact,       {.f = +0.05} },
      { MODKEY,                       XK_Return,               zoom,           {0} },
      { MODKEY,                       XK_Tab,                  view,           {0} },
      { MODKEY|ShiftMask,             XK_c,                    killclient,     {0} },
      { MODKEY,                       XK_t,                    setlayout,      {.v = &layouts[0]} },
      { MODKEY,                       XK_f,                    setlayout,      {.v = &layouts[1]} },
      { MODKEY,                       XK_m,                    setlayout,      {.v = &layouts[2]} },
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
