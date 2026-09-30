(* This file is part of the Dates_calc library. Copyright (C) 2022 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Aymeric Fromherz
   <aymeric.fromherz@inria.fr>, Raphaël Monat <raphael.monat@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

[@@@warning "-27"]

type date = { year : int; month : int; day : int }
(** A valid date in the standard Gregorian calendar. *)

type period = { years : int; months : int; days : int }
(** A period can be any number and combination of days, months, years. *)

exception InvalidDate
exception AmbiguousComputation

exception Overflow
(** A year beyond machine integers *)

type date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound
      (** When choosing [AbortOnRound], functions may raise
          [AmbiguousComputation]. *)

(** {2 Functions on periods}*)
let make_period ~(years : int) ~(months : int) ~(days : int) : period =
  { years; months; days }

let format_period (fmt : Format.formatter) (p : period) : unit =
  Format.fprintf fmt "[%d years, %d months, %d days]" p.years p.months p.days

let period_of_string str =
  try
    Scanf.sscanf str "[%d years, %d months, %d days]" (fun years months days ->
        make_period ~years ~months ~days)
  with Scanf.Scan_failure _ -> invalid_arg "period_of_string"

let add_periods (d1 : period) (d2 : period) : period =
  {
    years = d1.years + d2.years;
    months = d1.months + d2.months;
    days = d1.days + d2.days;
  }

let sub_periods (d1 : period) (d2 : period) : period =
  {
    years = d1.years - d2.years;
    months = d1.months - d2.months;
    days = d1.days - d2.days;
  }

let mul_period (d1 : period) (m : int) : period =
  { years = d1.years * m; months = d1.months * m; days = d1.days * m }

(** @raise [AmbiguousComputation]
      when the period is anything else than a number of days. *)
let period_to_days (p : period) : int =
  if p.years <> 0 || p.months <> 0 then raise AmbiguousComputation else p.days

(** {2 Functions on dates}*)

let is_leap_year (year : int) : bool =
  year mod 400 = 0 || (year mod 4 = 0 && year mod 100 <> 0)

(** @raise [InvalidDate]*)
let days_in_month ~(month : int) ~(is_leap_year : bool) : int =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year then 29 else 28
  | _ -> raise InvalidDate

let is_valid_date (d : date) : bool =
  try
    d.day >= 1
    && d.day <= days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year)
  with InvalidDate -> false

(** @raise [InvalidDate]*)
let make_date ~(year : int) ~(month : int) ~(day : int) : date =
  let d = { year; month; day } in
  if is_valid_date d then d else raise InvalidDate

(* Machine-integer arithmetic that raises [Overflow] instead of wrapping *)
let add_int (a : int) (b : int) : int =
  let r = a + b in
  if a >= 0 = (b >= 0) && r >= 0 <> (a >= 0) then raise Overflow else r

let neg_int (a : int) : int = if a = min_int then raise Overflow else -a

let mul_int (a : int) (b : int) : int =
  if a = 0 || b = 0 then 0
  else
    let r = a * b in
    if r / b <> a || (a = -1 && b = min_int) || (b = -1 && a = min_int) then
      raise Overflow
    else r

(* Division rounding towards minus infinity, for [b > 0] *)
let floor_div (a : int) (b : int) : int =
  if a >= 0 then a / b else -((-(a + 1) / b) + 1)

(* The Gregorian calendar repeats every 400 years, which have 146097 days *)
let days_in_400_years = 146097

(* The number of days from 0000-03-01 to [year-month-day], for small years
   (H. Hinnant's [days_from_civil]) *)
let day_number ~(year : int) ~(month : int) ~(day : int) : int =
  let y = if month <= 2 then year - 1 else year in
  let era = floor_div y 400 in
  let yoe = y - (era * 400) in
  let doy =
    (((153 * if month > 2 then month - 3 else month + 9) + 2) / 5) + day - 1
  in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * days_in_400_years) + doe

(* The inverse of [day_number] *)
let of_day_number (n : int) : int * int * int =
  let era = floor_div n days_in_400_years in
  let doe = n - (era * days_in_400_years) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = if mp < 10 then mp + 3 else mp - 9 in
  let year = yoe + (era * 400) + if month <= 2 then 1 else 0 in
  year, month, day

(* [year = cycles * 400 + y0] with [0 <= y0 < 400] *)
let split_year (year : int) : int * int =
  let cycles = floor_div year 400 in
  cycles, year - (cycles * 400)

(** Returns new [year, month]. Precondition: [1 <= month <= 12]
    @raise [Overflow] *)
let add_months_to_first_of_month_date
    ~(year : int)
    ~(month : int)
    ~(months : int) : int * int =
  let total = add_int (month - 1) months in
  let years = floor_div total 12 in
  add_int year years, total - (years * 12) + 1

(* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the last
   day of the current month. *)
let prev_valid_date (d : date) : date =
  assert (1 <= d.month && d.month <= 12);
  assert (1 <= d.day && d.day <= 31);
  if is_valid_date d then d
  else
    {
      d with
      day = days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year);
    }

(* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the
   first day of the next month. *)
let next_valid_date (d : date) : date =
  assert (1 <= d.month && d.month <= 12);
  assert (1 <= d.day && d.day <= 31);
  if is_valid_date d then d
  else
    let new_year, new_month =
      add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months:1
    in
    { year = new_year; month = new_month; day = 1 }

let round_date ~(round : date_rounding) (new_date : date) =
  if is_valid_date new_date then new_date
  else
    match round with
    | AbortOnRound -> raise AmbiguousComputation
    | RoundDown -> prev_valid_date new_date
    | RoundUp -> next_valid_date new_date

(** This function is only ever called from `add_dates` below. Hence, any call to
    `add_dates_years` will be followed by a call to `add_dates_month`. We
    therefore perform a single rounding in `add_dates_month`, to avoid
    introducing additional imprecision here, and to ensure that adding n years +
    m months is always equivalent to adding (12n + m) months *)
let add_dates_years ~(round : date_rounding) (d : date) (years : int) : date =
  { d with year = add_int d.year years }

let add_dates_month ~(round : date_rounding) (d : date) (months : int) : date =
  let new_year, new_month =
    add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months
  in
  let new_date = { d with year = new_year; month = new_month } in
  round_date ~round new_date

(* In constant time: whole 400-year cycles, then day numbers within a cycle *)
let add_dates_days (d : date) (days : int) : date =
  let cycles = floor_div days days_in_400_years in
  let days = days - (cycles * days_in_400_years) in
  let base, y0 = split_year d.year in
  let year, month, day =
    of_day_number (day_number ~year:y0 ~month:d.month ~day:d.day + days)
  in
  let year = add_int (add_int (mul_int base 400) year) (mul_int cycles 400) in
  { year; month; day }

(** @raise [AmbiguousComputation]
    @raise [Overflow] *)
let add_dates ?(round : date_rounding = AbortOnRound) (d : date) (p : period) :
    date =
  let d = add_dates_years ~round d p.years in
  (* NB: after add_dates_years, the date may not be correct. Rounding will be
     performed later, by add_dates_month *)
  let d = add_dates_month ~round d p.months in
  let d = add_dates_days d p.days in
  d

let compare_dates (d1 : date) (d2 : date) : int =
  if Int.compare d1.year d2.year = 0 then
    if Int.compare d1.month d2.month = 0 then Int.compare d1.day d2.day
    else Int.compare d1.month d2.month
  else Int.compare d1.year d2.year

(* The year: at least four digits, zero-padded, after a minus sign if it is
   negative (ISO 8601 within years 0 to 9999, and beyond them) *)

(** Respects ISO8601 format. *)
let format_date (fmt : Format.formatter) (d : date) : unit =
  let y = string_of_int d.year in
  let sign, digits =
    if y.[0] = '-' then "-", String.sub y 1 (String.length y - 1) else "", y
  in
  let pad = String.make (max 0 (4 - String.length digits)) '0' in
  Format.fprintf fmt "%s%s%s-%02d-%02d" sign pad digits d.month d.day

let date_of_string str =
  let invalid () = invalid_arg "date_of_string" in
  let n = String.length str in
  let is_digit c = c >= '0' && c <= '9' in
  let digits i len =
    if i + len > n then invalid ();
    for k = i to i + len - 1 do
      if not (is_digit str.[k]) then invalid ()
    done;
    int_of_string (String.sub str i len)
  in
  let negative = n > 0 && str.[0] = '-' in
  let y0 = if negative then 1 else 0 in
  let y1 =
    match String.index_from_opt str y0 '-' with
    | Some j -> j
    | None -> invalid ()
  in
  let year_digits = String.sub str y0 (y1 - y0) in
  let len = String.length year_digits in
  (* The canonical form only, as [format_date] writes it *)
  if
    len < 4
    || (len > 4 && year_digits.[0] = '0')
    || (negative && year_digits = "0000")
    || (not (String.for_all is_digit year_digits))
    || n <> y1 + 6
    || str.[y1 + 3] <> '-'
  then invalid ();
  let month = digits (y1 + 1) 2 and day = digits (y1 + 4) 2 in
  let year =
    match int_of_string_opt ((if negative then "-" else "") ^ year_digits) with
    | Some y -> y
    | None -> raise Overflow
  in
  make_date ~year ~month ~day

let first_day_of_month (d : date) : date =
  assert (is_valid_date d);
  make_date ~year:d.year ~month:d.month ~day:1

let last_day_of_month (d : date) : date =
  assert (is_valid_date d);
  let days_month =
    days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year)
  in
  make_date ~year:d.year ~month:d.month ~day:days_month

let neg_period (p : period) : period =
  { years = -p.years; months = -p.months; days = -p.days }

(** The returned [period] is always expressed as a number of days, computed in
    constant time.
    @raise [Overflow] *)
let sub_dates (d1 : date) (d2 : date) : period =
  let c1, y1 = split_year d1.year in
  let c2, y2 = split_year d2.year in
  let days =
    day_number ~year:y1 ~month:d1.month ~day:d1.day
    - day_number ~year:y2 ~month:d2.month ~day:d2.day
  in
  make_period ~years:0 ~months:0
    ~days:(add_int (mul_int (add_int c1 (neg_int c2)) days_in_400_years) days)

let date_to_ymd (d : date) : int * int * int = d.year, d.month, d.day
let period_to_ymds (p : period) : int * int * int = p.years, p.months, p.days
