package catala.stdlib;

import catala.runtime.*;
import catala.runtime.exception.CatalaError;
import java.math.BigInteger;

public class Decimal_internal {

    public static class Globals {

        // Rounding never computes a power of ten larger than needed
        private static final int MAX_POWER = 1000000;

        // k if the decimal expansion of 1/d has k digits, null otherwise
        private static Integer decimalDigits(BigInteger d) {
            int twos = 0, fives = 0;
            BigInteger five = BigInteger.valueOf(5);
            while (!d.testBit(0) && d.signum() != 0) {
                d = d.shiftRight(1);
                twos++;
            }
            while (d.mod(five).signum() == 0) {
                d = d.divide(five);
                fives++;
            }
            return d.equals(BigInteger.ONE) ? Math.max(twos, fives) : null;
        }

        public static final CatalaFunction<CatalaTuple, CatalaDecimal> roundToDecimal
                = tup_arg -> {
                    CatalaDecimal m = CatalaValue.<CatalaDecimal>cast(tup_arg.get(0));
                    BigInteger n = CatalaValue.<CatalaInteger>cast(tup_arg.get(1)).asBigInteger();
                    int cmp = n.compareTo(BigInteger.ZERO);
                    if (cmp == 0) {
                        return m.round().asDecimal();
                    }
                    if (cmp > 0) {
                        // A decimal with a finite expansion of at most n digits is
                        // returned as is; other powers beyond 10^MAX_POWER are
                        // IntegerOverflow
                        Integer digits = decimalDigits(m.getDenominator());
                        if (digits != null && n.compareTo(BigInteger.valueOf(digits)) >= 0) {
                            return m;
                        }
                        if (n.compareTo(BigInteger.valueOf(MAX_POWER)) > 0) {
                            throw CatalaError.error(CatalaError.Error.IntegerOverflow);
                        }
                        CatalaInteger pow_ten = new CatalaInteger(BigInteger.TEN.pow(n.intValue()));
                        return m.multiply(pow_ten).round().asDecimal().divide(pow_ten);
                    } else {
                        // |m| < 2^bits, so 10^k > 2 |m| when k > bits: zero
                        int bits = m.getNumerator().abs().divide(m.getDenominator())
                            .add(BigInteger.ONE).bitLength();
                        if (n.negate().compareTo(BigInteger.valueOf(bits)) > 0) {
                            return CatalaDecimal.of(0);
                        }
                        CatalaInteger pow_ten = new CatalaInteger(BigInteger.TEN.pow(n.negate().intValue()));
                        return m.divide(pow_ten).round().multiply(pow_ten).asDecimal();
                    }
                };
    }
}
