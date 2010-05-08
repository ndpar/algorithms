// Bottom-up dynamic programming

int TOTAL = 200

int[] coins = [1, 2, 5, 10, 20, 50, 100, 200]
int[] ways = new int[TOTAL + 1]
ways[0] = 1

coins.each { coin ->
    (coin..TOTAL).each { i ->
        ways[i] += ways[i - coin]
    }
}
 
println ways[TOTAL]

