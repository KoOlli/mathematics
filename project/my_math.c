#define MY_M_PI 3.141592653589793238462643383279502884197169399375105820974944
#define MY_EPS 1e-10
#define MY_NAN 0. / 0.
#define MY_INF 1.0 / 0.0
#include "my_math.h"

long double my_floor(double x) {
    long int int_answer = (long int)x;
    long double answer = (long double)int_answer;
    return (answer == x || x >= 0) ? answer : answer - 1;
}

double factorial(long long x) {
    x += 1;
    double ans = 1;
    for (int i = 1; i < x; i++) ans *= i;
    return ans;
}
long double long_factorial(long long x) {
    x += 1;
    long double ans = 1;
    for (int i = 1; i < x; i++) ans *= i;
    return ans;
}

long double pow_ie(long double base, long long exp) {
    long double res = 1.0;
    while (exp) {
        if ((exp & 1) != 0) {
            res *= base;
        }
        base *= base;
        exp >>= 1;
    }
    return res;
}
long double my_fabs(double x) {
    if (x < 0) x *= -1.0;
    return x;
}

long double my_exp(double x) {
    if (x >= 710) {
        return 1.0 / 0.0;
    }
    int flag = 0;
    if (x < 0) {
        x *= -1;
        flag = 1;
    }
    long double ans = 0;
    long double part = 1;
    for (double n = 0.0; part > MY_EPS; n++) {
        part = pow_ie(x, n) / factorial(n);
        ans += part;
    }
    if (flag) ans = 1.0 / ans;
    return ans;
}

long double my_log(double x) {
    if (x <= .0) return MY_NAN;
    if (x == 1.0) return 0.0;
    double yn;
    double yn_1 = 0.0;
    do {
        yn = yn_1;
        yn_1 = yn + 2 * ((x - my_exp(yn)) / (x + my_exp(yn)));
    } while (my_fabs(yn_1 - yn) > MY_EPS);
    return yn_1;
}

long double dfact(long int n) {
    long double f = 1;
    if (n % 2 == 0) {
        for (long int i = 2; i <= n; i += 2) {
            f *= i;
        }
    } else {
        for (long int i = 1; i <= n; i += 2) {
            f *= i;
        }
    }
    return f;
}

long int fact(long int n) {
    long int f = 1;
    for (long int i = 1; i <= n; i++) {
        f *= i;
    }
    return f;
}

long double my_pow(double base, double exponent) {
    long double result;
    if (base == 0.) {
        if (exponent > 0) {
            result = 0.;
        } else if (exponent == 0.) {
            result = 1.;
        } else {
            result = 1. / base;
        }
    } else if (exponent != (int) exponent && base < .0) {
        result = MY_NAN;
    } else {
        char flag = base < .0;
        if (flag) base *= -1;
        result = my_exp(exponent * my_log(base));
        if (flag && (long long) exponent % 2 != 0) result *= -1;
    }
    return result;
}

int my_abs(int x) {
    if (x < 0) {
        x = -x;
    }
    return x;
}
long double my_ceil(double x) {
    long int a = (long int)x;
    if (x != (double)a) {
        if ((double)x > 0) {
            a++;
        }
    }
    double b = (double)a;
    return b;
}
long double my_sin(long double x) {
    while (x > 2 * MY_M_PI) {
        x = x - 2 * MY_M_PI;
    }
    while (x < -2 * MY_M_PI) {
        x = x + 2 * MY_M_PI;
    }
    while (x < (-MY_M_PI) / 2) {
        x = x - 2 * (x + (MY_M_PI / 2));
    }
    while (x > MY_M_PI / 2) {
        x = x - 2 * (x - (MY_M_PI / 2));
    }
    while (x < (-MY_M_PI) / 2) {
        x = x - 2 * (x + (MY_M_PI / 2));
    }

    long double xx = x;
    int one = -1;
    long int i = 3;
    while (1) {
        long double iteration = (((pow_ie(x, i)) / (fact(i))) * one);
        i += 2;
        if (my_fabs(iteration) < MY_EPS) {
            break;
        }
        xx += iteration;
        one = one * -1;
    }
    return xx;
}
long double my_long_log(long double x) {
    if (x < 0) return MY_NAN;
    long double z = (x - 1.0) / (x + 1.0);
    long double result = 0;

    for (long long i = 3;; i += 2) {
        long double piece;
        piece = pow_ie(z, i) / (i);
        result += piece;
        if (i > 5000) break;
    }
    return 2 * (result + z);
}

long double my_long_exp(long double x) {
    if (x >= 710) {
        return 1.0 / 0.0;
    }
    long double ans = 0;
    for (long double n = 0.0; n < 155; n++)
        ans += pow_ie(x, n) / long_factorial(n);
    return ans;
}

long double my_powl(long double base, long double exp) {
    return my_long_exp(exp * my_long_log(base));
}

long double my_asin(double x) {
    int flagminus = 0;
    int flag0 = 0;
    int flag1 = 0;
    if (x < 0) {
        x = my_fabs(x);
        flagminus = 1;
    }
    double xx = x;
    if (x == 1) {
        flag1 = 1;
        xx = MY_M_PI / 2;
        if (flagminus == 1) {
            xx = -xx;
        }
    }
    if (x == 0) {
        flag0 = 1;
        xx = 0.0;
    }
    if ((flag0 == 1) || (flag1 == 1)) {
        return xx;
    }
    long int i = 1;
    while (1) {
        long double ddfact =
            ((dfact((long double)i)) / (dfact((long double)(i + 1))));
        long double lpow = ((my_powl((long double)x, (long double)(i + 2))) /
                            (long double)(i + 2));
        long double iteration = (ddfact * lpow);
        i += 2;
        if (my_fabs(iteration) < MY_EPS) {
            break;
        }
        if (i > 300) {
            break;
        }
        xx += iteration;
    }
    if (flagminus == 1) {
        xx = -xx;
    }
    return xx;
}

long double my_acos(double x) { return (MY_M_PI / 2) - my_asin(x); }

long double my_fmod(double x, double y) {
    // остаток операции деления с плавающей точкой
    y = my_fabs(y);
    long double res = my_fabs(x);
    if (x != x) {
        res = MY_NAN;
    }
    if (x == MY_INF || x == -MY_INF) {
        res = x;
    } else {
        res = (long double)((1 - 2 * (x < 0)) *
                            (my_fabs(x) - my_fabs(((int)(x / y)) * y)));
    }
    return res;
}

long double my_sqrt(double x) {  // квадратный корень
    double sqrt, temp;
    if (x != x || x < 0) {
        sqrt = MY_NAN;
    }
    sqrt = x / 2;
    temp = 0;
    while (sqrt != temp) {
        temp = sqrt;
        sqrt = (x / temp + temp) / 2;
    }
    return sqrt;
}

long long int my_factorial(long long int n) {
    long long int result;
    if (n == 0) {
        result = 1;
    } else {
        result = (n * my_factorial(n - 1));
    }
    return result;
}

long double my_cos(double x) {  // косинус
    return (my_sin(MY_M_PI / 2 + x));
}
long double my_atan(double x) {  // арктангенс
    return (my_asin(x / my_sqrt((x * x) + 1)));
}

long double my_tan(double x) { return (my_sin(x) / my_cos(x)); }

