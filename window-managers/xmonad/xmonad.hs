-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

 -- import XMonad.Layout.Magnifier
 -- import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing  

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

-- import XMonad.Actions.Volume

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure def)) defToggleStrutsKey
     $ someConfig
  
someConfig = def
  { layoutHook = someLayout
  , manageHook = someManageHook
  , startupHook = someStartup
  , normalBorderColor = "#384442"
  , focusedBorderColor = "#75905b"
  , borderWidth = 2
  }
  `additionalKeysP`
  [ ("M-S-<Return>", spawn            "st"                            )
  , ("C-S-q" ,       kill                                             )
  , ("M-S-l" ,       spawn            "xscreensaver-command -lock"    ) -- a letto
  , ("C-M-x" ,       spawn            "emacsclient --create-frame ~"  ) -- butterfly
  , ("M-S-p" ,       spawn            "pcmanfm ~"                     )
  , ("C-M-f" ,       spawn            "firefox"                       ) -- motherfucker
  , ("M-d"   ,       spawn            "rofi -show combi -show-icons"  )
  , ("C-M-s" ,       unGrab *> spawn  "scrot -s"                      )

  , ("<XF86MonBrightnessUp>"    , spawn  "light -A 0.5"                  )
  , ("<XF86MonBrightnessDown>"  , spawn  "light -U 0.5"                  )
  , ("<XF86AudioMute>"         , spawn  "amixer -q set Master toggle"   )
  , ("<XF86AudioLowerVolume>"  , spawn  "amixer set Master 5%-"         )
  , ("<XF86AudioRaiseVolume>"  , spawn  "amixer set Master 5%+"         )
  ] 

  
someLayout = spacingWithEdge 4 $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
  
someManageHook :: ManageHook
someManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    -- , className =? "stalonetray" --> doFloat
    , isDialog            --> doFloat
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
  
someStartup :: X()
someStartup = do
  spawn "stalonetray"
  spawn "emacs --daemon"
  spawn "feh --bg-center ~/Pictures/wallpapers/blissful.jpg"
  spawn "setxkbmap -layout us,it -option grp:win_space_toggle"
