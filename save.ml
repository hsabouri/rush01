type t = (int * int * int * int)

let is_digit = function '0' .. '9' -> true | _ -> false

let ft_string_all fct str =
        let rec loop len =
                if len >= 0 then begin
                        if fct (str.[len]) == false then
                                false
                        else
                                loop (len - 1)
                end
                        else
                                true
        in
        loop (String.length str - 1)

let checkDigit s = ft_string_all is_digit s
let checkSize s = let size = String.length s in
        size >= 1 && size <= 3 && (int_of_string s) <= 100

let createSome lst = Some (List.nth lst 0, List.nth lst 1, List.nth lst 2, List.nth lst 3)

let readSave str =
        if String.length str = 0 then None
        else begin
                let splited = String.split_on_char ' ' str in
                if List.length splited <> 4 || List.for_all checkDigit splited = false || List.for_all checkSize splited = false then None
                else createSome (List.map int_of_string splited)
        end


let openMe av =
        let ic = try open_in (av.(1)) with Sys_error _ -> open_in av.(0) in
        let ret = try input_line ic with End_of_file -> "" in
        close_in ic;
        ret

let openSave _ =
        let av = Sys.argv in
        if Array.length av <= 1 then None
        else if av.(1) <> "save.itama" then None
        else readSave (openMe av)

let string_of_t (a, b, c, d) =
        (string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ (string_of_int c) ^ " " ^ (string_of_int d)

let writeSave t =
        let oc = open_out "save.itama" in
        let s = string_of_t t in
        output oc (Bytes.of_string s) 0 (String.length s);
        close_out oc
