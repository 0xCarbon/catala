package catala.stdlib;

import catala.runtime.*;
import java.math.BigInteger;

public class Money_internal {

    public static class Globals {

        public static final CatalaFunction<CatalaTuple, CatalaMoney> roundToDecimal
                = tup_arg -> {
                    CatalaMoney m = CatalaValue.<CatalaMoney>cast(tup_arg.get(0));
                    BigInteger nth = CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger();
                    if (nth.compareTo(BigInteger.valueOf(2)) >= 0) {
                        return m;
                    }
                    if (nth.negate().compareTo(BigInteger.valueOf(m.asBigIntegerCents().abs().bitLength())) >= 0) {
                        // 10^-n > |m|: the quotient is 0, without computing the power
                        return CatalaMoney.ofCents(BigInteger.ZERO);
                    }
                    // -bitLength < n < 2: n fits an int
                    int n = nth.intValue();
                    BigInteger x = m.asBigIntegerCents();
                    CatalaInteger ten = CatalaInteger.of(10);
                    if (n == 1) {
                        CatalaPosition dummy_pos
                        = new CatalaPosition("none", 1, 1, 1, 1);
                        return m.multiply(ten).round().divide(dummy_pos, ten);
                    } else {
                        BigInteger pow_ten = ten.asBigInteger().pow(-n);
                        return CatalaMoney.ofCents(CatalaMoney.ofCents(x.divide(pow_ten)).round().asBigIntegerCents().multiply(pow_ten));
                    }
                };
    }
}
