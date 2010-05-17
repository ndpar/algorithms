/*
 * The maximum-subarray problem.
 * CLRS, p. 68.
 */

long[] array = [13,-3,-25,20,-3,-16,-23,18,20,-7,12,-5,-22,15,-4,7]

/*
 * Brute force Θ(n2) algorithm.
 * Works faster than devide and conquer for n < 6.
 */
def bruteForce(list) {
    long start = System.currentTimeMillis()
    def result = [left: 0, right: 0, sum: Integer.MIN_VALUE]
    for (int i in 0..<list.size()) {
        long sum = 0
        for (int j in i..<list.size()) {
            sum += list[j]
            if (result.sum < sum) {
                result.sum = sum
                result.left = i
                result.right = j
            }
        }
    }
    println "Brute force for ${list.size()} : ${System.currentTimeMillis() - start} ms"
    result
}

assert [left:7, right:10, sum:43L] == bruteForce(array)


/*
 * Devide and conquer Θ(n log n) algorithm.
 */
def maxSubArray(list) {
    long start = System.currentTimeMillis()
    def result = maxSubArray(list, 0, list.size() - 1)
    println "Devide and conquer for ${list.size()} : ${System.currentTimeMillis() - start} ms"
    result
}

def maxSubArray(a, low, high) {
    if (low == high) return [left: low, right: high, sum: a[low]]
    int mid = (low + high).intdiv(2)
    def left = maxSubArray(a, low, mid)
    def right = maxSubArray(a, mid + 1, high)
    def cross = maxCrossingSubArray(a, low, mid, high)
    if (left.sum >= right.sum && left.sum >= cross.sum)
        return left
    if (right.sum >= left.sum && right.sum >= cross.sum)
        return right
    cross
}

def maxCrossingSubArray(a, low, mid, high) {
    def left = [index: -1, sum: Integer.MIN_VALUE]
    long sum = 0
    for (int i in mid..low) {
        sum += a[i]
        if (left.sum < sum) {
            left.sum = sum
            left.index = i
        }
    }
    def right = [index: -1, sum: Integer.MIN_VALUE]
    sum = 0
    for (int j in mid+1..high) {
        sum += a[j]
        if (right.sum < sum) {
            right.sum = sum
            right.index = j
        }
    }
    [left: left.index, right: right.index, sum: left.sum + right.sum]
}

assert [left:7, right:10, sum:43L] == maxSubArray(array)


/*
 * Kadane's algorithm with linear running time.
 * http://en.wikipedia.org/wiki/Maximum_subarray_problem
 */
def kadane(list) {
    long start = System.currentTimeMillis()
    def result = [left: -1, right: -1, sum: 0L]
    long maxEndingHere = 0L
    int left = -1
    for (int i in 0..<list.size()) {
        long sum = maxEndingHere + list[i]
        if (0 < sum) {
            maxEndingHere = sum
            if (left < 0) left = i
        } else {
            maxEndingHere = 0
            left = -1
        }
        if (result.sum < maxEndingHere) {
            result.sum = maxEndingHere
            result.left = left
            result.right = i
        }
    }
    println "Kadane's algorithm ${list.size()} : ${System.currentTimeMillis() - start} ms"
    result
}

assert [left:7, right:10, sum:43L] == kadane(array)
