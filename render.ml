let pika = "
|█          █|    
  ██      ██      
  ██████████    ||
  ██  ██  ██   ██|
  ##██████##  ███ 
  █████████████   
  ██████████      
   ████████       
   ██    ██       
"

let pika_tired = "
      Z                 
           Z            
        Z             ||
 ███████##████████  ███|
██  ███ ████████  ███   
    ███████████████      
    ███ ████████        
████████##████████      
"

let background = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.white ; background = Some LTerm_style.white  }: LTerm_style.t)
let red = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.red ; background = Some LTerm_style.white  }: LTerm_style.t)
let black = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.black ; background = Some LTerm_style.white  }: LTerm_style.t)
let yellow = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.yellow ; background = Some LTerm_style.white  }: LTerm_style.t)
let ch = Zed_char.unsafe_of_utf8 "█"
let ch_z = Zed_char.unsafe_of_utf8 "Z"

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
