/* See LICENSE file for copyright and license details. */

/* xf86 keys
 * headers included in dwm.c, but all hail #ifndef and I just want to be explicit
 * ok ci riproviamo dopo
 * #include<X11/XF86keysym.h>
 */

/* appearance */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int gappx     = 10;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 0;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "monospace:size=10" };
static const char dmenufont[]       = "monospace:size=10";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray2_5[]     = "#666666"; /* il classicone */
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#005577";
static const char *colors[][3]      = {
    /*               fg         bg         border   */
    [SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
    [SchemeSel]  = { col_gray4, col_gray2, col_gray2_5 },
};

/* tagging */
static const int ntags = 5;
static const unsigned int max_tag_mask = 1 << ntags;
static const char *tags[] = { "0", "9", "8", "7", "6" };
/* poi usa questo per fare uno shift tag, ora vorrei fare LEESP */

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class         instance    title       tags mask     isfloating   monitor */
    { "Gimp",        NULL,       NULL,       0,            1,           -1 },
    { "Pavucontrol", NULL,       NULL,       0,            1,           -1 },
    { "Firefox",     NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
    /* symbol     arrange function */
    { "\\[T]/",      tile },     /* first entry is default */
    { "B:|"   ,      NULL },     /* no layout function means floating behavior */
    { "O_o"   ,      monocle },
};

/* key definitions */
#define MODKEY   Mod4Mask
#define ALTKEY   Mod1Mask
#define SHIFTKEY ShiftMask

#define CTLKEY ControlMask

#define NOMOD 0

#define TAGKEYS(KEY,TAG)      						\
    { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|Mod1Mask , KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf", col_gray4, NULL };
static const char *promptu[] = { "promptu" }; /* dmenu_run with picking directories */
static const char *termcmd[]  = { "st", "-e", "zsh", NULL };
static const char *stalcmd[]  = { "stallman", NULL }; /* emacs lauch script */

static const char *lightup[] = { "light","-A","0.5", NULL };
static const char *lightdown[] = { "light","-U","0.5", NULL };

static const char* volup[] = {"amixer", "set", "Master", "5%+", NULL};
static const char* voldown[] = {"amixer", "set", "Master", "5%-", NULL};
static const char* voltog[] = {"amixer", "-q" "set", "Master", "toggle", NULL};

static const Key keys[] = {
    /* modifier                     key        function        argument */

    /* for brightness and volume, see /usr/include/X11/XF86keysym.h */
    /* brightness */
    { MODKEY|ALTKEY|CTLKEY,          XK_k,     spawn, {.v = lightup}},
    { MODKEY|ALTKEY|CTLKEY,          XK_j,     spawn, {.v = lightdown}},

    /* volume */
    { MODKEY|ALTKEY|CTLKEY,          XK_l,     spawn, {.v = volup}},
    { MODKEY|ALTKEY|CTLKEY,          XK_h,     spawn, {.v = voldown}},
    { MODKEY|ALTKEY|CTLKEY,          XK_m,     spawn, {.v = voltog}},

    /* lauchers */
    { MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
    { MODKEY,                       XK_o,      spawn,          {.v = promptu } },
    { CTLKEY|ALTKEY,                XK_x,      spawn,          {.v = stalcmd } },
    { MODKEY,                       XK_Return, spawn,          {.v = termcmd } },

    /* layout */
    { MODKEY,                       XK_b,      togglebar,      {0} },
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
    { MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY|ShiftMask,             XK_Return, zoom,           {0} },
    { CTLKEY,                       XK_Tab,    view,           {0} },

    /* kill */
    { CTLKEY|ShiftMask,             XK_q,      killclient,     {0} },

    /* I don't have a right winkey */
    { CTLKEY|ALTKEY,                XK_t,      setlayout,      {.v = &layouts[0]} },
    { CTLKEY|ALTKEY,                XK_f,      setlayout,      {.v = &layouts[1]} },
    { CTLKEY|ALTKEY,                XK_m,      setlayout,      {.v = &layouts[2]} },
    { ALTKEY,                       XK_space,  setlayout,      {0} },
    { ALTKEY|ShiftMask,             XK_space,  togglefloating, {0} },
    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },

    /* monitors */
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    /* tags */
    /* this laptop only has a left windows key, and I like low tag numbers */
    TAGKEYS(                        XK_1,                      9)
    TAGKEYS(                        XK_2,                      8)
    TAGKEYS(                        XK_3,                      7)
    TAGKEYS(                        XK_4,                      6)
    TAGKEYS(                        XK_5,                      5)
    TAGKEYS(                        XK_6,                      4)
    TAGKEYS(                        XK_7,                      3)
    TAGKEYS(                        XK_8,                      2)
    TAGKEYS(                        XK_9,                      1)
    TAGKEYS(                        XK_0,                      0)
    { MODKEY|ShiftMask,             XK_q,      quit,           {0} },
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

