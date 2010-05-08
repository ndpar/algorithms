/*
 * The prime 41, can be written as the sum of six consecutive primes:
 *
 * 41 = 2 + 3 + 5 + 7 + 11 + 13
 *
 * This is the longest sum of consecutive primes that adds to a prime
 * below one-hundred.
 *
 * The longest sum of consecutive primes below one-thousand that adds
 * to a prime, contains 21 terms, and is equal to 953.
 *
 * Which prime, below one-million, can be written as the sum of the
 * most consecutive primes?
 */

MAX = 1000000

primes = []
new File('primes_below_1m.txt').eachLine {
    primes << it.toInteger()
}
HashSet<Integer> sprimes = new HashSet<Integer>(primes)


begin = System.currentTimeMillis()

int number = 0, length = 0

for (int start in 0..<primes.size()) {
    int sum = 0
    for (int i in start..<primes.size()) {
        sum += primes[i]
        if (sum > MAX) break // this is critical point - without this line it will take much longer
        if (sprimes.contains(sum) && length < i - start + 1) {
            number = sum
            length = i - start + 1
        }
    }
}

println "$length: $number (${System.currentTimeMillis()-begin}ms)"
