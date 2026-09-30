open Catala_runtime

(* Toplevel def of_ymd *)
let of_ymd : code_location -> integer -> integer -> integer -> date =
 fun pos y m d ->
  let invalid () =
    let pad n z =
      if Z.fits_int z then Printf.sprintf "%0*d" n (Z.to_int z)
      else Z.to_string z
    in
    raise
      (Error
         ( DateError
             (Printf.sprintf "|%s-%s-%s| is not a valid date" (pad 4 y)
                (pad 2 m) (pad 2 d)),
           [pos],
           None ))
  in
  if not (Z.fits_int y && Z.fits_int m && Z.fits_int d) then invalid ()
  else
    try
      Dates_calc.make_date ~year:(Z.to_int y) ~month:(Z.to_int m)
        ~day:(Z.to_int d)
    with Dates_calc.InvalidDate -> invalid ()

(* Toplevel def to_ymd *)
let to_ymd : date -> integer * integer * integer =
 fun (d : date) ->
  let y, m, d = Dates_calc.date_to_ymd d in
  Z.of_int y, Z.of_int m, Z.of_int d

(* Toplevel def last_day_of_month *)
let last_day_of_month : date -> date =
 fun (d : date) -> Dates_calc.last_day_of_month d

(* Toplevel def add_rounded_down *)
let add_rounded_down : date -> duration -> date =
 fun (d : date) (dur : duration) ->
  Dates_calc.add_dates d dur ~round:Dates_calc.RoundDown

(* Toplevel def add_rounded_up *)
let add_rounded_up : date -> duration -> date =
 fun (d : date) (dur : duration) ->
  Dates_calc.add_dates d dur ~round:Dates_calc.RoundUp

let () =
  Catala_runtime.register_module "Date_internal"
    [
      "of_ymd", Stdlib.Obj.repr of_ymd;
      "to_ymd", Stdlib.Obj.repr to_ymd;
      "last_day_of_month", Stdlib.Obj.repr last_day_of_month;
      "add_rounded_down", Stdlib.Obj.repr add_rounded_down;
      "add_rounded_up", Stdlib.Obj.repr add_rounded_up;
    ]
    "*external*"
