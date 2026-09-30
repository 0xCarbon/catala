#include <catala_runtime.h>
#include <gmp.h>

/* Rounding to [n] decimal places never computes a power of ten larger than
   needed: a decimal with a finite expansion of at most [n] digits is returned
   as is, a power of ten beyond twice the value rounds it to zero, and other
   powers beyond 10^1000000 are IntegerOverflow. */
#define MAX_POWER 1000000

CATALA_DEC DecimalInternal__round_to_decimal(CATALA_DEC variable, CATALA_INT nth_decimal)
{
  mpq_ptr result;
  CATALA_DEC round;
  mpq_ptr pow_ten_q;
  unsigned long int exp;
  int cmp = mpz_sgn(nth_decimal);

  if (cmp == 0)
    return o_round_rat(variable);

  if (cmp > 0) {
    mpz_t d, two, five;
    unsigned long int twos, fives, digits;
    int finite;
    mpz_init_set(d, mpq_denref(variable));
    mpz_init_set_ui(two, 2);
    mpz_init_set_ui(five, 5);
    twos = mpz_remove(d, d, two);
    fives = mpz_remove(d, d, five);
    finite = mpz_cmp_ui(d, 1) == 0;
    digits = twos > fives ? twos : fives;
    mpz_clear(d); mpz_clear(two); mpz_clear(five);
    if (finite && mpz_cmp_ui(nth_decimal, digits) >= 0)
      return variable;
    if (mpz_cmp_ui(nth_decimal, MAX_POWER) > 0)
      catala_error(catala_integer_overflow, NULL, 0, NULL);
    exp = mpz_get_ui(nth_decimal);
  } else {
    mpz_t bound;
    unsigned long int bits;
    /* |x| < 2^bits, so 10^k > 2 |x| when k > bits */
    mpz_init(bound);
    mpz_tdiv_q(bound, mpq_numref(variable), mpq_denref(variable));
    mpz_abs(bound, bound);
    mpz_add_ui(bound, bound, 1);
    bits = mpz_sizeinbase(bound, 2);
    mpz_clear(bound);
    if (mpz_cmp_si(nth_decimal, -(signed long int) bits) < 0) {
      result = catala_malloc(sizeof(__mpq_struct));
      mpq_init(result);
      return result;
    }
    exp = (unsigned long int) (-mpz_get_si(nth_decimal));
  }

  result = catala_malloc(sizeof(__mpq_struct));
  mpq_init(result);
  pow_ten_q = catala_malloc(sizeof(__mpq_struct));
  mpq_init(pow_ten_q);
  mpz_ui_pow_ui(mpq_numref(pow_ten_q), 10, exp);

  if (cmp > 0) {
    mpq_mul(result, variable, pow_ten_q);
    round = o_round_rat(result);
    mpq_div(result, round, pow_ten_q);
  } else {
    mpq_div(result, variable, pow_ten_q);
    round = o_round_rat(result);
    mpq_mul(result, round, pow_ten_q);
  }

  mpq_canonicalize(result);
  return result;
}
