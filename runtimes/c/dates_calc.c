/* This file is part of the Dates_calc library. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>, Raphaël Monat
   <raphael.monat@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. */

#include <assert.h>
#include <limits.h>
#include <stdio.h>

#define BOOL int
#define FALSE 0
#define TRUE 1

/* Layout of this C version:
   - types and functions in this module are prefixed with [dc_]
   - dates and periods are manipulated as pointers to the defined structs
   - functions return through a first [ret] pointer argument, the other
     arguments are [const].
   - functions that can fail return the [dc_success] type. What is stored into
     [ret] is unspecified when that is [dc_error].
   - it is expected that [ret] and other arguments may overlap, so, in the code
     below, a field in [ret] should never be written before the same field in
     any other argument of the same type is read.
*/

typedef enum dc_success {
  dc_error, dc_ok,
  dc_overflow /* a year or a number of days beyond long int */
} dc_success;

typedef enum dc_date_rounding {
  dc_date_round_up,
  dc_date_round_down,
  dc_date_round_abort
} dc_date_rounding;


typedef struct dc_date {
  long int year;
  unsigned long int month;
  unsigned long int day;
} dc_date;

typedef struct dc_period {
  long int years;
  long int months;
  long int days;
} dc_period;

void dc_make_period(dc_period *ret, const long int y, const long int m, const long int d) {
  ret->years = y;
  ret->months = m;
  ret->days = d;
}

void dc_print_period (const dc_period *p) {
  printf("[%ld years, %ld months, %ld days]", p->years, p->months, p->days);
}

dc_success dc_period_of_string (dc_period *ret, const char* s) {
  if
    (sscanf(s, "[%ld years, %ld months, %ld days]",
            &ret->years, &ret->months, &ret->days)
     == 3)
    return dc_ok;
  else
    return dc_error;
}

void dc_add_periods (dc_period *ret, const dc_period *p1, const dc_period *p2) {
  ret->years = p1->years + p2->years;
  ret->months = p1->months + p2->months;
  ret->days = p1->days + p2->days;
}

void dc_sub_periods (dc_period *ret, const dc_period *p1, const dc_period *p2) {
  ret->years = p1->years - p2->years;
  ret->months = p1->months - p2->months;
  ret->days = p1->days - p2->days;
}

void dc_mul_periods (dc_period *ret, const dc_period *p, const long int m) {
  ret->years = p->years * m;
  ret->months = p->months * m;
  ret->days = p->days * m;
}

dc_success dc_period_to_days (long int *ret, const dc_period *p) {
  if (p->years || p->months) return dc_error;
  else *ret = p->days;
  return dc_ok;
}

BOOL dc_is_leap_year (const long int y) {
  return (y % 400 == 0 || (y % 4 == 0 && y % 100 != 0));
}

unsigned long int dc_days_in_month (const dc_date *d) {
  switch (d->month) {
  case 2:
    return (dc_is_leap_year(d->year) ? 29 : 28);
  case 4: case 6: case 9: case 11:
    return 30;
  default:
    return 31;
  }
}

BOOL dc_is_valid_date (const dc_date *d) {
  return (1 <= d->day && d->day <= dc_days_in_month(d));
}

dc_success dc_make_date(dc_date *ret, const long int y, const unsigned long int m, const unsigned long int d) {
  ret->year = y;
  ret->month = m;
  ret->day = d;
  if (dc_is_valid_date(ret)) return dc_ok;
  else return dc_error;
}

void dc_copy_date(dc_date *ret, const dc_date *d) {
  if (ret != d) {
    ret->year = d->year;
    ret->month = d->month;
    ret->day = d->day;
  }
}

/* Precondition: [1 <= d->month <= 12]. The returned day is always [1] */
/* Arithmetic on long ints that reports [dc_overflow] instead of wrapping */
static dc_success add_long (long int *ret, long int a, long int b) {
  if ((b > 0 && a > LONG_MAX - b) || (b < 0 && a < LONG_MIN - b))
    return dc_overflow;
  *ret = a + b;
  return dc_ok;
}

/* For [c > 0] */
static dc_success mul_long (long int *ret, long int a, long int c) {
  if (a > LONG_MAX / c || a < LONG_MIN / c) return dc_overflow;
  *ret = a * c;
  return dc_ok;
}

/* Division rounding towards minus infinity, for [b > 0] */
static long int floor_div (long int a, long int b) {
  return a >= 0 ? a / b : -((-(a + 1)) / b) - 1;
}

/* The Gregorian calendar repeats every 400 years, which have 146097 days */
#define DAYS_IN_400_YEARS 146097L

/* The number of days from 0000-03-01 to [year-month-day], for small years
   (H. Hinnant's [days_from_civil]) */
static long int day_number (long int year, long int month, long int day) {
  long int y = month <= 2 ? year - 1 : year;
  long int era = floor_div(y, 400);
  long int yoe = y - era * 400;
  long int doy = (153 * (month > 2 ? month - 3 : month + 9) + 2) / 5 + day - 1;
  long int doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  return era * DAYS_IN_400_YEARS + doe;
}

/* The inverse of [day_number] */
static void of_day_number (dc_date *ret, long int n) {
  long int era = floor_div(n, DAYS_IN_400_YEARS);
  long int doe = n - era * DAYS_IN_400_YEARS;
  long int yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  long int doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  long int mp = (5 * doy + 2) / 153;
  long int month = mp < 10 ? mp + 3 : mp - 9;
  ret->day = doy - (153 * mp + 2) / 5 + 1;
  ret->month = month;
  ret->year = yoe + era * 400 + (month <= 2 ? 1 : 0);
}

/* The first day of the month [months] after that of [d], in constant time */
dc_success dc_add_months(dc_date *ret, const dc_date *d, const long int months) {
  long int total, years;
  if (add_long(&total, (long int) d->month - 1, months) != dc_ok)
    return dc_overflow;
  years = floor_div(total, 12);
  if (add_long(&ret->year, d->year, years) != dc_ok) return dc_overflow;
  ret->month = total - years * 12 + 1;
  ret->day = 1;
  return dc_ok;
}

/* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the last
   day of the current month. */
void dc_prev_valid_date (dc_date *ret, const dc_date *d) {
  assert (1 <= d->month && d->month <= 12);
  assert (1 <= d->day && d->day <= 31);
  if (dc_is_valid_date(d))
    dc_copy_date(ret, d);
  else {
    ret->year = d->year;
    ret->month = d->month;
    ret->day = dc_days_in_month(d);
  }
}

/* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the
   first day of the next month. */
dc_success dc_next_valid_date (dc_date *ret, const dc_date *d) {
  assert (1 <= d->month && d->month <= 12);
  assert (1 <= d->day && d->day <= 31);
  if (dc_is_valid_date(d)) {
    dc_copy_date(ret, d);
    return dc_ok;
  } else
    return dc_add_months (ret, d, 1);
}

dc_success dc_round_date (dc_date *ret, const dc_date_rounding rnd, const dc_date *d) {
  if (dc_is_valid_date(d)) {
    dc_copy_date(ret, d);
    return dc_ok;
  } else switch (rnd) {
    case dc_date_round_down:
      dc_prev_valid_date(ret, d);
      return dc_ok;
    case dc_date_round_up:
      return dc_next_valid_date(ret, d);
    default:
      return dc_error;
    }
}

/* In constant time: whole 400-year cycles, then day numbers within a cycle */
static dc_success add_dates_days (dc_date *ret, const dc_date *d, const long int days) {
  long int cycles = floor_div(days, DAYS_IN_400_YEARS);
  long int rest = days - cycles * DAYS_IN_400_YEARS;
  long int base = floor_div(d->year, 400);
  long int y0 = d->year - base * 400;
  long int year, shift;
  of_day_number(ret, day_number(y0, d->month, d->day) + rest);
  if (mul_long(&year, base, 400) != dc_ok
      || add_long(&year, year, ret->year) != dc_ok
      || mul_long(&shift, cycles, 400) != dc_ok
      || add_long(&year, year, shift) != dc_ok)
    return dc_overflow;
  ret->year = year;
  return dc_ok;
}

dc_success dc_add_dates (dc_date *ret, const dc_date_rounding rnd, const dc_date *d, const dc_period *p) {
  dc_success success;
  dc_date tmp;
  if (add_long(&tmp.year, d->year, p->years) != dc_ok) return dc_overflow;
  tmp.month = d->month;
  /* NB: at this point, the date may not be correct.
     Rounding is performed after add_months */
  if (dc_add_months(&tmp, &tmp, p->months) != dc_ok) return dc_overflow;
  tmp.day = d->day;
  success = dc_round_date(ret, rnd, &tmp);
  if (success == dc_ok)
    return add_dates_days(ret, ret, p->days);
  else
    return success;
}

int dc_compare_dates (const dc_date *d1, const dc_date *d2) {
  long int cmp;
  cmp = d1->year - d2->year;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = d1->month - d2->month;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = d1->day - d2->day;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  return 0;
}

/* YYYY-MM-DD (ISO 8601) for the years 0 to 9999; beyond them the year has more
   digits, and a negative year has a minus sign followed by at least four
   digits: -0738-02-03, 2737909006-12-28 */
void dc_print_date (const dc_date *d) {
  unsigned long abs_year =
    d->year < 0 ? 0UL - (unsigned long)d->year : (unsigned long)d->year;
  printf("%s%04lu-%02lu-%02lu", d->year < 0 ? "-" : "", abs_year, d->month,
         d->day);
}

/* Reads exactly the form dc_print_date writes */
dc_success dc_date_of_string (dc_date *ret, const char* s) {
  int negative = (s[0] == '-');
  const char *y = s + negative, *p = y;
  unsigned long abs_year = 0;
  size_t len;
  while (*p >= '0' && *p <= '9') {
    if (abs_year > (ULONG_MAX - 9) / 10) return dc_overflow;
    abs_year = abs_year * 10 + (unsigned long)(*p - '0');
    p++;
  }
  len = (size_t)(p - y);
  if (len < 4 || (len > 4 && y[0] == '0') || (negative && abs_year == 0))
    return dc_error;
  if (!(p[0] == '-' && p[1] >= '0' && p[1] <= '9' && p[2] >= '0' && p[2] <= '9'
        && p[3] == '-' && p[4] >= '0' && p[4] <= '9' && p[5] >= '0'
        && p[5] <= '9' && p[6] == '\0'))
    return dc_error;
  if (abs_year > (negative ? (unsigned long)LONG_MAX + 1UL : (unsigned long)LONG_MAX))
    return dc_overflow;
  ret->year = negative ? -(long)(abs_year - 1) - 1 : (long)abs_year;
  ret->month = (unsigned long)((p[1] - '0') * 10 + (p[2] - '0'));
  ret->day = (unsigned long)((p[4] - '0') * 10 + (p[5] - '0'));
  return dc_is_valid_date(ret) ? dc_ok : dc_error;
}

void dc_first_day_of_month (dc_date *ret, const dc_date *d) {
  assert(dc_is_valid_date(d));
  ret->year = d->year;
  ret->month = d->month;
  ret->day = 1;
}

void dc_last_day_of_month (dc_date *ret, const dc_date *d) {
  assert(dc_is_valid_date(d));
  ret->year = d->year;
  ret->month = d->month;
  ret->day = dc_days_in_month(d);
}

void dc_neg_period (dc_period *ret, const dc_period *p) {
  ret->years = - p->years;
  ret->months = - p->months;
  ret->days = - p->days;
}

/* The returned [period] is always expressed as a number of days, computed in
   constant time. */
dc_success dc_sub_dates (dc_period *ret, const dc_date *d1, const dc_date *d2) {
  long int c1 = floor_div(d1->year, 400), c2 = floor_div(d2->year, 400);
  long int days = day_number(d1->year - c1 * 400, d1->month, d1->day)
    - day_number(d2->year - c2 * 400, d2->month, d2->day);
  long int cycles;
  ret->years = 0;
  ret->months = 0;
  if (c2 == LONG_MIN || add_long(&cycles, c1, -c2) != dc_ok
      || mul_long(&cycles, cycles, DAYS_IN_400_YEARS) != dc_ok
      || add_long(&ret->days, cycles, days) != dc_ok)
    return dc_overflow;
  return dc_ok;
}

long int dc_date_year(const dc_date *d) {
  return d->year;
}

unsigned long int dc_date_month(const dc_date *d) {
  return d->month;
}

unsigned long int dc_date_day(const dc_date *d) {
  return d->day;
}

long int dc_period_years(const dc_period *p) {
  return p->years;
}

long int dc_period_months(const dc_period *p) {
  return p->months;
}

long int dc_period_days(const dc_period *p) {
  return p->days;
}

