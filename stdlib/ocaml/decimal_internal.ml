open Catala_runtime

(* Rounding to [n] decimal places never computes a power of ten larger than
   needed: a decimal with a finite expansion of at most [n] digits is returned
   as is, a power of ten beyond twice the value rounds it to zero, and other
   powers beyond 10^1000000 are IntegerOverflow. *)
let max_power = 1_000_000

(* [Some k] if the decimal expansion of [1/d] has [k] digits *)
let decimal_digits (d : Z.t) : int option =
  let d, twos = Z.remove d (Z.of_int 2) in
  let d, fives = Z.remove d (Z.of_int 5) in
  if Z.equal d Z.one then Some (max twos fives) else None

(* Toplevel round_to_decimal *)
let round_to_decimal : decimal -> integer -> decimal =
 fun m n ->
  if Z.equal n Z.zero then Q.of_bigint (round m)
  else if Z.gt n Z.zero then
    match decimal_digits (Q.den m) with
    | Some k when Z.geq n (Z.of_int k) -> m
    | _ ->
      if Z.gt n (Z.of_int max_power) then
        raise (Error (IntegerOverflow, [], None))
      else
        let pow_10 = Q.of_bigint (Z.pow (Z.of_int 10) (Z.to_int n)) in
        Q.div (Q.of_bigint (round (Q.mul m pow_10))) pow_10
  else
    let k = Z.neg n in
    (* |m| < 2^bits, so 10^k > 2 |m| when k > bits *)
    let bits = Z.numbits (Z.succ (Q.to_bigint (Q.abs m))) in
    if Z.gt k (Z.of_int bits) then Q.zero
    else
      let pow_10 = Q.of_bigint (Z.pow (Z.of_int 10) (Z.to_int k)) in
      Q.mul (Q.of_bigint (round (Q.div m pow_10))) pow_10

let () =
  Catala_runtime.register_module "Decimal_internal"
    ["round_to_decimal", Obj.repr round_to_decimal]
    "*external*"
