#include <catala_runtime.h>
#include <gmp.h>

CATALA_MONEY MoneyInternal__round_to_decimal(CATALA_MONEY variable, CATALA_INT nth_decimal)
{
  signed long int nth_int;
  if (mpz_cmp_si(nth_decimal, 2) >= 0) return variable;
  /* 10^-n > |m|: the quotient is 0, without computing the power */
  if (mpz_sgn(variable) == 0
      || mpz_cmp_si(nth_decimal,
                    -(signed long int) mpz_sizeinbase(variable, 2)) <= 0)
    return catala_new_int(0);
  /* -bits < n < 2: n fits a long */
  nth_int = mpz_get_si(nth_decimal);
  {
    CATALA_INT ten = catala_new_int(10);
    if (nth_int == 1) {
      CATALA_MONEY m = o_round_mon(o_mult_mon_int(variable, ten));
      return o_div_mon_int(NULL, m, ten);
    } else {
      mpz_ptr pow_ten = catala_malloc(sizeof(__mpz_struct));
      mpz_init(pow_ten);
      mpz_ui_pow_ui(pow_ten, 10, (unsigned long int) (-nth_int));
      return o_mult_mon_int(o_round_mon(o_div_mon_int(NULL, variable, pow_ten)), pow_ten);
    }
  }
}
