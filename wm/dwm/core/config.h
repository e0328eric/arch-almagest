/* Almagest dwm config file
 * See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const unsigned int gappih    = 13;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 13;       /* vert inner gap between windows */
static const unsigned int gappoh    = 13;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 13;       /* vert outer gap between windows and screen edge */
static const int smartgaps          = 1;        /* 1 means no outer gap when there is only one window */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = {
    "Noto Sans CJK KR:size=9",
    "LiberationMono:fontformat=truetype:size=12:antialias=true",
    "SpaceMono Nerd Font:fontformat=truetype:size=10:antialias=true",
    "SpaceMono Nerd Font Mono:pixselsize=9",
    "Siji:fontformat=truetype:size=12:antialias=true",
};
static const char dmenufont[]       = "Noto Sans CJK KR:size=9";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#268bd2";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
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
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.5; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
	{ "|M|",      centeredmaster },
	{ ">M>",      centeredfloatingmaster },
};

/* key definitions */
#define ModMask Mod4Mask
#define AltMask Mod1Mask
#define TAGKEYS(KEY,TAG) \
	{ ModMask,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ ModMask|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ ModMask|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ ModMask|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define TERMCMD(cmd) { .v = (const char*[]){ "konsole", "-e", cmd, NULL } }
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_extended_run", NULL };
static const char *termcmd[]  = { "st", NULL };
static const char *browsercmd[]  = { "firefox", NULL };
//static const char *emacscmd[]  = { "emacs", NULL };
static const char *vscodecmd[]  = { "code-insiders", NULL };
static const char *officecmd[]  = { "onlyoffice", NULL };
static const char *guifilecmd[]  = { "krusader", NULL };
static const char *soundcloudcmd[] = { "auryo", NULL };
static const char *screenAcmd[]  = { "spectacle", "-r", NULL };
static const char *screenBcmd[]  = { "spectacle", "-r", NULL };
static const char *screenCcmd[]  = { "spectacle", "-u", NULL };
static const char *poweroffcmd[] = { "check-twice.sh", "Are you sure you want to shutdown?", "poweroff", NULL };
static const char *rebootcmd[] = { "check-twice.sh", "Are you sure you want to reboot?", "reboot", NULL };

#include "movestack.c"
static Key keys[] = {
    /* modifier                     key        function        argument */
    { ControlMask|AltMask,          XK_s,      spawn,          {.v = dmenucmd } },
    { ModMask,                      XK_Return, spawn,          {.v = termcmd } },
    { ModMask|ShiftMask,            XK_Return, spawn,          {.v = browsercmd } },
    { ControlMask|ShiftMask,        XK_Return, spawn,          TERMCMD("mocp") },
    //{ ModMask,                      XK_e,      spawn,          {.v = emacscmd } },
    { ModMask,                      XK_e,      spawn,          {.v = vscodecmd } },
    { ModMask|ControlMask,          XK_Return, spawn,          {.v = soundcloudcmd } },
    { ModMask,                      XK_f,      spawn,          TERMCMD("vifm") },
    { ModMask,                      XK_F1,     spawn,          {.v = guifilecmd } },
    { ModMask,                      XK_o,      spawn,          {.v = officecmd } },
    { ModMask|ShiftMask,            XK_x,      quit,           {0} },
    { ModMask|ShiftMask,            XK_c,      spawn,          {.v = poweroffcmd } },
    { ModMask|ShiftMask,            XK_v,      spawn,          {.v = rebootcmd } },
    { 0,                            XK_Print,  spawn,          {.v = screenAcmd } },
    { ControlMask,                  XK_Print,  spawn,          {.v = screenBcmd } },
    { ControlMask|ShiftMask,        XK_Print,  spawn,          {.v = screenCcmd } },
    { ModMask,                      XK_F12,    spawn,          SHCMD("wine ~/.wine/drive_c/Program\\ Files/Kakao/KakaoTalk/KakaoTalk.exe") },
    { 0,                          0x1008ff13,  spawn,          SHCMD("pactl set-sink-volume @DEFAULT_SINK@ +1% && resbar") },
    { 0,                          0x1008ff11,  spawn,          SHCMD("pactl set-sink-volume @DEFAULT_SINK@ -1% && resbar") },
    { 0,                          0x1008ff12,  spawn,          SHCMD("pactl set-sink-mute @DEFAULT_SINK@ toggle && resbar") },
    { 0,                          0x1008ff02,  spawn,          SHCMD("xbacklight -inc 10 && resbar") },
    { 0,                          0x1008ff03,  spawn,          SHCMD("xbacklight -dec 10 && resbar") },
    { ModMask,                       XK_b,      togglebar,      {0} },
    { ModMask,                       XK_Right,  focusstack,     {.i = +1 } },
    { ModMask,                       XK_Left,   focusstack,     {.i = -1 } },
    { ModMask|ShiftMask,             XK_Right,  movestack,     {.i = +1 } },
    { ModMask|ShiftMask,             XK_Left,   movestack,     {.i = -1 } },
    { ModMask,                       XK_comma,  incnmaster,     {.i = +1 } },
    { ModMask,                       XK_period, incnmaster,     {.i = -1 } },
    { AltMask,                      XK_Left,   setmfact,       {.f = -0.01} },
    { AltMask,                      XK_Right,  setmfact,       {.f = +0.01} },
    { AltMask|ShiftMask,            XK_Left,   setmfact,       {.f = -0.05} },
    { AltMask|ShiftMask,            XK_Right,  setmfact,       {.f = +0.05} },
    { ModMask|AltMask,               XK_Up,     incrgaps,       {.i = +1 } },
    { ModMask|AltMask,               XK_Down,   incrgaps,       {.i = -1 } },
    { ModMask|AltMask|ShiftMask,     XK_Up,     incrogaps,      {.i = +1 } },
    { ModMask|AltMask|ShiftMask,     XK_Down,   incrogaps,      {.i = -1 } },
    { ModMask|AltMask|ControlMask,   XK_Up,     incrigaps,      {.i = +1 } },
    { ModMask|AltMask|ControlMask,   XK_Down,   incrigaps,      {.i = -1 } },
    { ModMask|AltMask,               XK_0,      togglegaps,     {0} },
    { ModMask|AltMask|ShiftMask,     XK_0,      defaultgaps,    {0} },
    { ModMask,                       XK_w,      incrihgaps,     {.i = +1 } },
    { ModMask,                       XK_s,      incrihgaps,     {.i = -1 } },
    { ModMask|ControlMask,           XK_w,      incrivgaps,     {.i = +1 } },
    { ModMask|ControlMask,           XK_s,      incrivgaps,     {.i = -1 } },
    { ModMask|AltMask,               XK_w,      incrohgaps,     {.i = +1 } },
    { ModMask|AltMask,               XK_s,      incrohgaps,     {.i = -1 } },
    { ModMask|ShiftMask,             XK_w,      incrovgaps,     {.i = +1 } },
    { ModMask|ShiftMask,             XK_s,      incrovgaps,     {.i = -1 } },
    { ModMask|ShiftMask,             XK_e,      zoom,           {0} },
    { ModMask|ShiftMask,             XK_q,      killclient,     {0} },
    { ModMask|ShiftMask,             XK_t,      setlayout,      {.v = &layouts[0]} },
    { ModMask|ShiftMask,             XK_f,      setlayout,      {.v = &layouts[1]} },
    { ModMask|ShiftMask,             XK_m,      setlayout,      {.v = &layouts[2]} },
    { ModMask|ShiftMask,             XK_u,      setlayout,      {.v = &layouts[3]} },
    { ModMask|ShiftMask,             XK_o,      setlayout,      {.v = &layouts[4]} },
    { ModMask|AltMask,               XK_space,  setlayout,      {0} },
    { ModMask,                       XK_space,  togglefloating, {0} },
    { ModMask,                       XK_0,      view,           {.ui = ~0 } },
    { ModMask|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
    { ModMask|ControlMask,           XK_comma,  focusmon,       {.i = -1 } },
    { ModMask|ControlMask,           XK_period, focusmon,       {.i = +1 } },
    { ModMask|ControlMask|ShiftMask, XK_comma,  tagmon,         {.i = -1 } },
    { ModMask|ControlMask|ShiftMask, XK_period, tagmon,         {.i = +1 } },
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask          button          function        argument */
    { ClkLtSymbol,          0,                  Button1,        setlayout,      {0} },
    { ClkLtSymbol,          0,                  Button3,        setlayout,      {.v = &layouts[2]} },
    { ClkWinTitle,          0,                  Button2,        zoom,           {0} },
    { ClkStatusText,        0,                  Button2,        spawn,          {.v = termcmd } },
    { ClkClientWin,         ModMask,             Button1,        movemouse,      {0} },
    { ClkClientWin,         ModMask,             Button2,        togglefloating, {0} },
    { ClkClientWin,         ModMask|ShiftMask,   Button1,        resizemouse,    {0} },
    { ClkTagBar,            0,                  Button1,        view,           {0} },
    { ClkTagBar,            0,                  Button3,        toggleview,     {0} },
    { ClkTagBar,            ModMask,             Button1,        tag,            {0} },
    { ClkTagBar,            ModMask,             Button3,        toggletag,      {0} },
};

