package catala.stdlib;

import catala.runtime.*;
import catala.runtime.exception.*;

public class Date_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaTuple, CatalaDate> ofYmd
                = tup_arg -> {
                    CatalaPosition pos = CatalaValue.<CatalaPosition>cast(tup_arg.get(0));
                    CatalaInteger dyear = CatalaValue.<CatalaInteger>cast(tup_arg.get(1));
                    CatalaInteger dmonth = CatalaValue.<CatalaInteger>cast(tup_arg.get(2));
                    CatalaInteger dday = CatalaValue.<CatalaInteger>cast(tup_arg.get(3));
                    try {
                        return CatalaDate.of(dyear.asBigInteger().intValueExact(),
                                dmonth.asBigInteger().intValueExact(),
                                dday.asBigInteger().intValueExact());
                    } catch (IllegalArgumentException | ArithmeticException e) {
                        throw CatalaError.error(CatalaError.Error.DateError, pos);
                    }
                };

        public static final CatalaFunction<CatalaDate, CatalaTuple> toYmd
                = d -> {
                    return new CatalaTuple(new CatalaInteger[]{d.getYear(), d.getMonth(), d.getDay()});
                };

        public static final CatalaFunction<CatalaDate, CatalaDate> lastDayOfMonth
                = d -> {
                    return d.getLastDayOfMonth();
                };

        public static final CatalaFunction<CatalaTuple, CatalaDate> addRoundedDown
                = tup_arg_22 -> {
                    CatalaDate d = CatalaValue.<CatalaDate>cast(tup_arg_22.get(0));
                    CatalaDuration dur = CatalaValue.<CatalaDuration>cast(tup_arg_22.get(1));
                    return d.addDurationRoundDown(new CatalaPosition("", 0, 0, 0, 0), dur);
                };

        public static final CatalaFunction<CatalaTuple, CatalaDate> addRoundedUp
                = tup_arg_23 -> {
                    CatalaDate d = CatalaValue.<CatalaDate>cast(tup_arg_23.get(0));
                    CatalaDuration dur = CatalaValue.<CatalaDuration>cast(tup_arg_23.get(1));
                    return d.addDurationRoundUp(new CatalaPosition("", 0, 0, 0, 0), dur);
                };
    }
}
