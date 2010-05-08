/*
 * It is possible to write five as a sum in exactly six different ways:
 *
 * 4 + 1
 * 3 + 2
 * 3 + 1 + 1
 * 2 + 2 + 1
 * 2 + 1 + 1 + 1
 * 1 + 1 + 1 + 1 + 1
 *
 * How many different ways can one hundred be written as a sum of at least two positive integers?
 */ 

/*
 * Brute-force using definition.
 * Works for numbers upto 15.
 */
def exponential(int max) {
    sums = [[2,1],[1,1,1]]
    4.upto(12) { n ->
        c = []
        sums.each { r ->
            0.upto(r.size()-1) { i ->
                nr = r.clone()
                nr[i]++
                nr.sort()
                if (!c.contains(nr)) c << nr
            }
        }
        sums = c + [ones(n)]
    }
    sums.size()
}

def ones(n) {
    a = []; n.times() { a << 1 }; a
}

/*
 * Bottom-up dynamic programming.
 * Works for numbers upto 10,000.
 */
def quadratic(int max) {
    p = new BigInteger[max+1]
    p[0] = 1; 1.upto(max) { p[it] = 0 as BigInteger }

    1.upto(max) { i ->
        i.upto(max) { j ->
            p[j] += p[j-i]
        }
    }
    p[-1] - 1
}

/*
 * Euler's formula.
 * http://en.wikipedia.org/wiki/Partition_(number_theory)
 * http://www.math.temple.edu/~melkamu/html/partition.pdf
 * Works for numbers upto 50,000.
 */
def fast(int max) {
    p = new BigInteger[max + 1]
    p[0] = 1

    1.upto(max) { n ->
        int i = 1, k = 1; BigInteger pn = 0, sign = 1
        while (i > 0) {
            if ((i = n - f1(k)) >= 0) pn += sign * p[i]
            if ((i = n - f2(k)) >= 0) pn += sign * p[i]
            sign *= -1; k++
        }
        p[n] = pn
    }
    p[-1] - 1
}

int f1(int k) {
    (int) k * (3 * k - 1) / 2
}

int f2(int k) {
    (int) k * (3 * k + 1) / 2
}


println fast(100)
