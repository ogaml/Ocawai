open OgamlGraphics

type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  border_color      : Color.t;
  active_color      : Color.t}


let blue_theme =
  { bar_color       = Color.(`RGB RGB.{ r = 0.22 ; g = 0.3   ; b = 0.57 ; a = 1. }) ;
    default_color   = Color.(`RGB RGB.{ r = 0.73 ; g = 0.85  ; b = 0.95 ; a = 1. }) ;
    highlight_color = Color.(`RGB RGB.{ r = 0.54 ; g = 0.7   ; b = 0.89 ; a = 1. }) ;
    border_color    = Color.(`RGB RGB.{ r = 0.47 ; g = 0.625 ; b = 0.78 ; a = 1. }) ;
    active_color    = Color.(`RGB RGB.{ r = 0.12 ; g = 0.45  ; b = 0.74 ; a = 1. }) }


let yellow_theme =
  { bar_color       = Color.(`RGB RGB.{ r = 0.35 ; g = 0.35 ; b = 0.04 ; a = 1. }) ;
    default_color   = Color.(`RGB RGB.{ r = 0.74 ; g = 0.74 ; b = 0.51 ; a = 1. }) ;
    highlight_color = Color.(`RGB RGB.{ r = 0.61 ; g = 0.61 ; b = 0.43 ; a = 1. }) ;
    border_color    = Color.(`RGB RGB.{ r = 0.51 ; g = 0.51 ; b = 0.39 ; a = 1. }) ;
    active_color    = Color.(`RGB RGB.{ r = 0.47 ; g = 0.47 ; b = 0.  ; a = 1. }) }

let red_theme =
  { bar_color       = Color.(`RGB RGB.{ r = 0.39  ; g = 0.04 ; b = 0.04 ; a = 1. }) ;
    default_color   = Color.(`RGB RGB.{ r = 0.78  ; g = 0.47 ; b = 0.47 ; a = 1. }) ;
    highlight_color = Color.(`RGB RGB.{ r = 0.625 ; g = 0.43 ; b = 0.43 ; a = 1. }) ;
    border_color    = Color.(`RGB RGB.{ r = 0.59  ; g = 0.39 ; b = 0.39 ; a = 1. }) ;
    active_color    = Color.(`RGB RGB.{ r = 0.51  ; g = 0.  ;  b = 0.   ; a = 1. }) }
