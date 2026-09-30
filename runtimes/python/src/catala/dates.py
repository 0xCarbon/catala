#   This file is part of the Dates_calc library. Copyright (C) 2023 Inria,
#   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Aymeric Fromherz
#   <aymeric.fromherz@inria.fr>, Raphaël Monat <raphael.monat@inria.fr>
#
#   Licensed under the Apache License, Version 2.0 (the "License"); you may not
#   use this file except in compliance with the License. You may obtain a copy of
#   the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#   License for the specific language governing permissions and limitations under
#   the License.

# This implementation is derived from the OCaml implementation dates.ml.
# The latter acts as the reference implementation in case of any discrepancy

from __future__ import annotations
from collections import namedtuple
from functools import total_ordering, partial 
from enum import Enum
from copy import deepcopy
import re
from typing import NewType, List, Generic, Callable, Tuple, TypeVar, Iterable, Union, Any

DateRounding = Enum("DateRounding", ["RoundUp", "RoundDown", "AbortOnRound"])
# A dynamic way to make AbortOnRound & co top-level globals:
globals().update(DateRounding.__members__)

class InvalidDate(Exception): pass

class AmbiguousComputation(Exception):
    """When choosing `AbortOnRound`, functions may raise this exception"""
    pass

# Utilities

def is_leap_year(year : int) -> bool:
    return (year % 400 == 0) or (year % 4 == 0 and year % 100 != 0)

def days_in_month(*, month : int, is_leap_year : bool) -> int:
    if month in [1, 3, 5, 7, 8, 10, 12]: return 31
    elif month in [4, 6, 9, 11]: return 30
    elif month == 2:
        return 29 if is_leap_year else 28
    else:
        raise InvalidDate

def add_months_to_first_of_month_date(*, year : int, month : int, months : int) -> tuple[int, int]:
    """
    Returns new `year, month`, in constant time.
    Precondition: `1 <= month <= 12`
    """
    years, new_month = divmod(month - 1 + months, 12)
    return year + years, new_month + 1

# The Gregorian calendar repeats every 400 years, which have 146097 days
DAYS_IN_400_YEARS = 146097

def day_number(year : int, month : int, day : int) -> int:
    """The number of days from 0000-03-01 to `year-month-day`
    (H. Hinnant's `days_from_civil`)"""
    y = year - 1 if month <= 2 else year
    era, yoe = divmod(y, 400)
    doy = (153 * (month - 3 if month > 2 else month + 9) + 2) // 5 + day - 1
    doe = yoe * 365 + yoe // 4 - yoe // 100 + doy
    return era * DAYS_IN_400_YEARS + doe

def of_day_number(n : int) -> tuple[int, int, int]:
    """The inverse of `day_number`"""
    era, doe = divmod(n, DAYS_IN_400_YEARS)
    yoe = (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365
    doy = doe - (365 * yoe + yoe // 4 - yoe // 100)
    mp = (5 * doy + 2) // 153
    day = doy - (153 * mp + 2) // 5 + 1
    month = mp + 3 if mp < 10 else mp - 9
    return yoe + era * 400 + (1 if month <= 2 else 0), month, day

# A hack to do custom infix operators, documented here: https://tomerfiliba.com/blog/Infix-Operators
class Infix(object):
    def __init__(self, func):
        self.func = func
    def __or__(self, other):
        return self.func(other)
    def __ror__(self, other):
        return Infix(partial(self.func, other))
    def __call__(self, v1, v2):
        return self.func(v1, v2)

# this decorator will automatically create the other comparison methods based
# on the implementation of < and ==
@total_ordering 
class Date:
    def __init__(self, *, year, month, day):
        self.year = year
        self.month = month
        self.day = day
        if not self.is_valid:
            raise InvalidDate

    @property
    def is_valid(self):
        try:
            return self.day >= 1 and self.day <= days_in_month(month=self.month, is_leap_year=is_leap_year(self.year))
        except InvalidDate:
            return False

    def prev_valid_date(self) -> Date:
        """If the date is valid, does nothing. We expect the month number to be always
        valid when calling this. If the date is invalid due to the day number,
        then this function rounds down: if the day number is >= days_in_month,
        to the last day of the current month.

        """
        assert(1 <= self.month and self.month <= 12)
        assert(1 <= self.day and self.day <= 31)
        if self.is_valid: return self
        else:
            return Date(year = self.year,
                        month = self.month,
                        day = days_in_month(month = self.month,
                                            is_leap_year = is_leap_year(self.year)))

    def next_valid_date(self) -> Date:
        """ If the date is valid, does nothing. We expect the month number to be
        always valid when calling this. If the date is invalid due to the day
        number, then this function rounds down: if the day number is >=
        days_in_month, to the first day of the next month.
        """
        assert(1 <= self.month and self.month <= 12)
        assert(1 <= self.day and self.day <= 31)
        if self.is_valid: return self
        else:
            new_year, new_month = add_months_to_first_of_month_date(year = self.year,
                                                                    month = self.month,
                                                                    months = 1)
            return Date(year = new_year,
                        month = new_month,
                        day = 1)

    def first_day_of_month(self) -> Date:
        assert(self.is_valid)
        return Date(year = self.year, month = self.month, day = 1)

    def last_day_of_month(self) -> Date:
        assert(self.is_valid)
        return Date(year = self.year, month = self.month,
                    day = days_in_month(month = self.month,
                                        is_leap_year = is_leap_year(self.year)))

    def round(self, round : DateRounding) -> Date:
        if self.is_valid: return self
        else:
            if round == DateRounding.AbortOnRound: raise AmbiguousComputation
            elif round == DateRounding.RoundDown: return self.prev_valid_date()
            elif round == DateRounding.RoundUp: return self.next_valid_date()

    # This function is only ever called from `add_dates` below.
    # Hence, any call to `add_dates_years` will be followed by a call
    # to `add_dates_month`. We therefore perform a single rounding
    # in `add_dates_month`, to avoid introducing additional imprecision here,
    # and to ensure that adding n years + m months is always equivalent to
    # adding (12n + m) months
    def add_dates_years(self, years : int, round : DateRounding) -> Date:
        # Let's deepcopy to avoid the valid date check performed in init
        new_date = deepcopy(self)
        new_date.year = self.year + years
        return new_date

    def add_dates_months(self, months : int, round : DateRounding) -> Date:
        new_year, new_month = add_months_to_first_of_month_date(year = self.year,
                                                                month = self.month,
                                                                months = months)
        # Let's deepcopy to avoid the valid date check performed in init
        new_date = deepcopy(self)
        new_date.year = new_year
        new_date.month = new_month
        return new_date.round(round)

    def add_dates_days(self, days : int) -> Date:
        # In constant time, with day numbers
        year, month, day = of_day_number(day_number(self.year, self.month, self.day) + days)
        return Date(year = year, month = month, day = day)

    def __add__(self, p : Period, round : DateRounding = DateRounding.AbortOnRound) -> Date:
        d = self.add_dates_years(p.years, round)
        # NB: after add_dates_years, the date may not be correct.
        # Rounding will be performed later, by add_dates_month
        d = d.add_dates_months(p.months, round)
        d = d.add_dates_days(p.days)
        return d

    def __sub__(self, other : Date) -> Period:
        # In constant time: always a number of days
        return Period(years = 0, months = 0,
                      days = day_number(self.year, self.month, self.day)
                      - day_number(other.year, other.month, other.day))

    def __eq__(self, other):
        return self.year == other.year \
            and self.month == other.month \
            and self.day == other.day

    def __lt__(self, other):
        if self.year == other.year:
            if self.month == other.month: return self.day < other.day
            else: return self.month < other.month
        else: return self.year < other.year

    @property
    def ymd(self):
        return self.year, self.month, self.day

    def __str__(self):
        """Respects ISO8601 format."""
        return f"{self.year:04}-{self.month:02}-{self.day:02}"

    def __repr__(self):
        return '<{} object at {}: {}>'.format(
            self.__class__.__name__,
            hex(id(self)),
            str(self))

    @classmethod
    def from_string(self, s : str) -> Date:
        rege = re.compile("([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])")
        match = rege.fullmatch(s)
        if match is None:
            raise InvalidDate()
        else:
            d = int(match[3])
            m = int(match[2])
            y = int(match[1])
            return Date(year = y, month = m, day = d)

# custom infix operators for Date addition.
# You can thus do `l +addup+ p`!
@Infix
def addup(l : Date, p : Period) -> Date:
    if not (isinstance(l, Date) and isinstance(p, Period)): raise TypeError("+up+ requires a date and a period")
    return l.__add__(p, DateRounding.RoundUp)

@Infix
def adddown(l : Date, p : Period) -> Date:
    if not (isinstance(l, Date) and isinstance(p, Period)): raise TypeError("+down+ requires a date and a period")
    return l.__add__(p, DateRounding.RoundDown)


class Period:
    def __init__(self, *, years, months, days):
        self.years = years
        self.months = months
        self.days = days

    def __add__(self, other: Period) -> Period:
        return Period(years = self.years + other.years,
                      months = self.months + other.months,
                      days = self.days + other.days)

    def __sub__(self, other: Period) -> Period:
        return Period(years = self.years - other.years,
                      months = self.months - other.months,
                      days = self.days - other.days)

    def __mul__(self, m: int) -> Period:
        return Period(years = self.years * m,
                      months = self.months * m,
                      days = self.days * m)

    def to_days(self) -> int:
        if self.years != 0 or self.months != 0:
            raise AmbiguousComputation
        else:
            return self.days

    def __str__(self):
        return f"[{self.years} years, {self.months} months, {self.days} days]"

    def __repr__(self):
        return '<{} object at {}: {}>'.format(
            self.__class__.__name__,
            hex(id(self)),
            str(self))

    @classmethod
    def from_string(self, s : str) -> Period:
        rege = re.compile("\\[([0-9]+) years, ([0-9]+) months, ([0-9]+) days\\]")
        match = rege.fullmatch(s)
        if match is None:
            raise InvalidDate()
        else:
            d = int(match[3])
            m = int(match[2])
            y = int(match[1])
            return Period(years = y, months = m, days = d)

    def __neg__(self) -> Period:
        return Period(years = - self.years,
                      months = - self.months,
                      days = - self.days)

    @property
    def ymds(self):
        return self.years, self.months, self.days

