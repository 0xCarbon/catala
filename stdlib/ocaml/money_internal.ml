open Catala_runtime

(* Toplevel round_to_decimal *)
let round_to_decimal : money -> integer -> money =
 fun m n ->
  if Z.geq n (Z.of_int 2) then m
  else if Z.geq (Z.neg n) (Z.of_int (Z.numbits m)) then
    (* 10^-n > |m|: the quotient is 0, without computing the power *)
    Z.zero
  else
    (* -numbits m < n < 2: n fits a machine integer *)
    let n_int = Z.to_int n in
    let ten = Z.of_int 10 in
    if n_int = 1 then Z.(money_round (m * ten) / ten)
    else
      let pow_10 = Z.pow ten (-n_int) in
      Z.(money_round (m / pow_10) * pow_10)

let () =
  Catala_runtime.register_module "Money_internal"
    ["round_to_decimal", Obj.repr round_to_decimal]
    "*external*"
