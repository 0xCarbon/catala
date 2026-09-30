from catala_runtime import *

# Rounding to [n] decimal places never computes a power of ten larger than
# needed: a decimal with a finite expansion of at most [n] digits is returned
# as is, a power of ten beyond twice the value rounds it to zero, and other
# powers beyond 10^1000000 are IntegerOverflow.
MAX_POWER = 1000000

def decimal_digits(d: int) -> int | None:
    twos = 0
    while d % 2 == 0:
        d //= 2
        twos += 1
    fives = 0
    while d % 5 == 0:
        d //= 5
        fives += 1
    return max(twos, fives) if d == 1 else None

def round_to_decimal(variable: Decimal, n_decimal: Integer) -> Decimal:
    n = int(n_decimal)
    if n == 0:
        return variable.round()
    if n > 0:
        k = decimal_digits(variable.denominator)
        if k is not None and n >= k:
            return variable
        if n > MAX_POWER:
            raise IntegerOverflow(None)
        pow_10 = Decimal(10 ** n)
        return Decimal(variable * pow_10).round() / pow_10
    bits = (abs(variable.numerator) // variable.denominator + 1).bit_length()
    if -n > bits:
        return Decimal(0)
    pow_10 = Decimal(10 ** (-n))
    return Decimal(variable / pow_10).round() * pow_10
