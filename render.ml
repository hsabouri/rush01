let pika = "
 BY        YB
  YY    YY
  YYYYYYYYYY   BB
  YYBBYYBBYY  YYB
  RRYYYYYYRR YYY
  YYYYYYYYYYYY
  YYYYYYYYYY
   YYYYYYYY
   YY    YY
"

let pika_tired = "
      Z
           Z
        Z             BB
 YYYYYYYRRYYYYYYYY  YYYB
YY  YYYBYYYYYYYY  YYY   
    YYYYYYYYYYYYYYY      
    YYYBYYYYYYYY        
YYYYYYYYRRYYYYYYYY      
"

let background = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.white ; background = Some LTerm_style.white  }: LTerm_style.t)
let background_green = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.green ; background = Some LTerm_style.green  }: LTerm_style.t)
let red = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.red ; background = Some LTerm_style.white  }: LTerm_style.t)
let green = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.green ; background = Some LTerm_style.white  }: LTerm_style.t)
let black = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.black ; background = Some LTerm_style.white  }: LTerm_style.t)
let yellow = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.yellow ; background = Some LTerm_style.white  }: LTerm_style.t)
let ch = Zed_char.unsafe_of_utf8 "█"
let ch_z = Zed_char.unsafe_of_utf8 "Z"

let draw_bar ctx (size: LTerm_geom.size) color (t, name) n =
	let rec loop i = match i with
		| i when i < (LTerm_draw.size ctx).cols - 1 && i < t -> LTerm_draw.draw_char ctx (size.rows / 2 + n * 2 - 3) i ~style:color ch ; loop (i + 1)
		| _ -> ()
	in loop 1
    ; LTerm_draw.draw_hline ctx (size.rows / 2 + n * 2 - 2) 0 ((LTerm_draw.size ctx).cols) LTerm_draw.Light
    ; LTerm_draw.draw_string_aligned ctx (size.rows / 2 + n * 2 - 3) LTerm_geom.H_align_center ~style:black name

let draw_image ctx (size: LTerm_geom.size) img =
	let rec loop img i x y = match i with
		| i when i < String.length img -> (match String.get img i with
			| '\n' -> loop img (i + 1) 0 (y + 1)
			| 'R' -> LTerm_draw.draw_char ctx (y + 4) (x + size.cols / 2 - 8) ~style:red ch ; loop img (i + 1) (x + 1) y
			| 'B' -> LTerm_draw.draw_char ctx (y + 4) (x + size.cols / 2 - 8) ~style:black ch ; loop img (i + 1) (x + 1) y
			| 'Y' -> LTerm_draw.draw_char ctx (y + 4) (x + size.cols / 2 - 8) ~style:yellow ch ; loop img (i + 1) (x + 1) y
			| 'Z' -> LTerm_draw.draw_char ctx (y + 4) (x + size.cols / 2 - 8) ~style:black ch_z ; loop img (i + 1) (x + 1) y
			| _ -> loop img (i + 1) (x + 1) y
		)
		| _ -> ()
	in loop img 0 0 0

let filter img (sick, dirty, sad) = img
