open OgamlGraphics

type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  border_color      : Color.t;
  active_color      : Color.t}


let blue_theme =
  { bar_color       = Color.(`RGB { .22 ; .3 ; .57 ; 1. }) ;
    default_color   = Color.(`RGB { .73 ; .85 ; .95 ; 1. }) ;
    highlight_color = Color.(`RGB { .54 ; .7 ; .89 ; 1. }) ;
    border_color    = Color.(`RGB { .47 ; .625 ; .78 ; 1. }) ;
    active_color    = Color.(`RGB { .12 ; .45 ; .74 ; 1. }) }


let yellow_theme =
  { bar_color       = Color.(`RGB { .35 ; .35 ; .04 ; 1. }) ;
    default_color   = Color.(`RGB { .74 ; .74 ; .51 ; 1. }) ;
    highlight_color = Color.(`RGB { .61 ; .61 ; .43 ; 1. }) ;
    border_color    = Color.(`RGB { .51 ; .51 ; .39 ; 1. }) ;
    active_color    = Color.(`RGB { .47 ; .47 ; 0. ; 1. }) }

let red_theme =
  { bar_color       = Color.(`RGB { .39 ; .04 ; .04 ; 1. }) ;
    default_color   = Color.(`RGB { .78 ; .47 ; .47 ; 1. }) ;
    highlight_color = Color.(`RGB { .625 ; .43 ; .43 ; 1. }) ;
    border_color    = Color.(`RGB { .59 ; .39 ; .39 ; 1. }) ;
    active_color    = Color.(`RGB { .51 ; 0. ; 0. ; 1. }) }
