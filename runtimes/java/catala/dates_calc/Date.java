package catala.dates_calc;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Note that Date is purposefully non-final (we want to inherit from it in the
 * java catala runtime, in order to add a marker interface)
 */
public class Date implements Comparable<Date> {

    public enum Rounding {
        ROUND_UP, ROUND_DOWN, ABORT_ON_ROUND
    }

    public final int year;
    public final int month;
    public final int day;

    private Date(int year, int month, int day) {
        // We need to be able to privately create
        // invalid dates prior to rounding.
        // The user-facing API (`of`) will throw an error
        // if such a date is created, however

        this.year = year;
        this.month = month;
        this.day = day;
    }

    // Exactly the form toString writes
    private static final Pattern DATE_PATTERN =
        Pattern.compile("(?!-0000-)(-?(?:\\d{4}|[1-9]\\d{4,}))-(\\d{2})-(\\d{2})");

    public static Date fromString(String s) {
        Matcher matcher = DATE_PATTERN.matcher(s);
        if (matcher.matches()) {
            // Beyond a machine integer: NumberFormatException
            int year = Integer.parseInt(matcher.group(1));
            int month = Integer.parseInt(matcher.group(2));
            int day = Integer.parseInt(matcher.group(3));
            return Date.of(year, month, day);
        } else {
            throw new IllegalArgumentException("Invalid date format: " + s);
        }
    }


    public static final Date of(int year, int month, int day) {
        if (!isValid(year, month, day)) {
            throw new IllegalArgumentException("Invalid date");
        }

        return new Date(year, month, day);
    }

    public final Date firstDayOfMonth() {
        return Date.of(this.year, this.month, 1);
    }

    public final Date lastDayOfMonth() {
        return of(this.year, this.month, daysInMonth(this.month, isLeapYear(this.year)));
    }

    private boolean isValid() {
        return isValid(this.year, this.month, this.day);
    }

    private final Date round(Rounding mode) {
        if (this.isValid()) {
            return this;
        } else {
            switch (mode) {
                case ROUND_UP:
                    return nextValidDate();
                case ROUND_DOWN:
                    return prevValidDate();
                case ABORT_ON_ROUND:
                    throw new AmbiguousComputationException("Ambiguous date computation involving potential rounding");
            }
            throw new AssertionError("Unreachable code reached in Date.round");
        }
    }

    private static boolean isLeapYear(int year) {
        return (year % 400 == 0) || (year % 4 == 0 && year % 100 != 0);
    }

    private static int daysInMonth(int month, boolean isLeapYear) {
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException("Invalid month number");
        }

        switch (month) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                return 31;
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
            case 2:
                if (isLeapYear) {
                    return 29;
                } else {
                    return 28;
                }
        }

        throw new RuntimeException("This path should be unreachable, this is a bug");

    }

    private static boolean isValid(int year, int month, int day) {
        return day >= 1 && day <= daysInMonth(month, isLeapYear(year)) && month >= 1 && month <= 12;
    }

    private Date prevValidDate() {
        assert (this.month >= 1 && this.month <= 12);
        assert (this.day >= 1 && this.day <= 31);
        if (this.isValid()) {
            return this;
        } else {
            return of(this.year, this.month, daysInMonth(this.month, isLeapYear(this.year)));
        }

    }

    // The Gregorian calendar repeats every 400 years, which have 146097 days
    private static final long DAYS_IN_400_YEARS = 146097;

    // The number of days from 0000-03-01 to year-month-day, for small years
    // (H. Hinnant's days_from_civil)
    private static long dayNumber(long year, int month, int day) {
        long y = month <= 2 ? year - 1 : year;
        long era = Math.floorDiv(y, 400L);
        long yoe = y - era * 400;
        long doy = (153L * (month > 2 ? month - 3 : month + 9) + 2) / 5 + day - 1;
        long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        return era * DAYS_IN_400_YEARS + doe;
    }

    // The inverse of dayNumber: {year, month, day}
    private static long[] ofDayNumber(long n) {
        long era = Math.floorDiv(n, DAYS_IN_400_YEARS);
        long doe = n - era * DAYS_IN_400_YEARS;
        long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        long mp = (5 * doy + 2) / 153;
        long day = doy - (153 * mp + 2) / 5 + 1;
        long month = mp < 10 ? mp + 3 : mp - 9;
        return new long[]{yoe + era * 400 + (month <= 2 ? 1 : 0), month, day};
    }

    // In constant time; a year beyond int raises ArithmeticException
    private int[] addMonthsToFirstOfMonthDate(int year, int month, int plusMonths) {
        assert (month >= 1 && month <= 12);
        long total = (long) month - 1 + plusMonths;
        long years = Math.floorDiv(total, 12L);
        return new int[]{Math.toIntExact(year + years), (int) (total - years * 12) + 1};
    }

    private Date nextValidDate() {
        assert (this.month >= 1 && this.month <= 12);
        assert (this.day >= 1 && this.day <= 31);
        if (this.isValid()) {
            return this;
        } else {
            int[] yearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, 1);
            return of(yearAndMonth[0], yearAndMonth[1], 1);
        }
    }

    /* This function is only ever called from `add_dates` below.
     Hence, any call to `add_dates_years` will be followed by a call
     to `add_dates_month`. We therefore perform a single rounding
     in `add_dates_month`, to avoid introducing additional imprecision here,
     and to ensure that adding n years + m months is always equivalent to
     adding (12n + m) months
     */
    private Date addYears(int years) {
        return new Date(Math.addExact(this.year, years), this.month, this.day);
    }

    private Date addMonths(int months, Rounding rounding) {
        int[] newYearAndMonth = addMonthsToFirstOfMonthDate(this.year, this.month, months);
        return new Date(newYearAndMonth[0], newYearAndMonth[1], this.day).round(rounding);
    }

    // In constant time: whole 400-year cycles, then day numbers within a cycle
    private Date addDays(int days) {
        long cycles = Math.floorDiv((long) days, DAYS_IN_400_YEARS);
        long rest = days - cycles * DAYS_IN_400_YEARS;
        long base = Math.floorDiv((long) this.year, 400L);
        long[] r = ofDayNumber(dayNumber(this.year - base * 400, this.month, this.day) + rest);
        return of(Math.toIntExact(base * 400 + r[0] + cycles * 400), (int) r[1], (int) r[2]);
    }

    public final Date add(Period p, Rounding rounding){
      Date d = this.addYears(p.years);
      d = d.addMonths(p.months, rounding);
      d = d.addDays(p.days);
      return d;
    }

    public final Date add(Period p){
        return this.add(p, Rounding.ABORT_ON_ROUND);
    }

    // In constant time; a number of days beyond int raises ArithmeticException
    public final Period sub(Date d){
        long c1 = Math.floorDiv((long) this.year, 400L);
        long c2 = Math.floorDiv((long) d.year, 400L);
        long days = (c1 - c2) * DAYS_IN_400_YEARS
            + dayNumber(this.year - c1 * 400, this.month, this.day)
            - dayNumber(d.year - c2 * 400, d.month, d.day);
        return new Period(0, 0, Math.toIntExact(days));
    }

    @Override
    public int compareTo(Date other) {
        assert this.isValid() && other.isValid();

        int yearComparison = Integer.compare(this.year, other.year);
        if (yearComparison != 0) {
            return yearComparison;
        }
        int monthComparison = Integer.compare(this.month, other.month);
        if (monthComparison != 0) {
            return monthComparison;
        }
        return Integer.compare(this.day, other.day);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Date date = (Date) obj;
        return year == date.year && month == date.month && day == date.day;
    }

    @Override
    public int hashCode() {
        int result = Integer.hashCode(year);
        result = 31 * result + Integer.hashCode(month);
        result = 31 * result + Integer.hashCode(day);
        return result;
    }

    @Override
    public String toString() {
        // YYYY-MM-DD (ISO 8601) for the years 0 to 9999; beyond them the year
        // has more digits, and a negative year has a minus sign followed by at
        // least four digits: -0738-02-03, 2737909006-12-28
        String abs = Long.toString(Math.abs((long) this.year));
        return (this.year < 0 ? "-" : "") + "0".repeat(Math.max(0, 4 - abs.length()))
            + abs + String.format("-%02d-%02d", this.month, this.day);
    }
}
