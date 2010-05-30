// ------------------------------------------------------------------
// Comparison algorithms. Decision-tree model.
// ------------------------------------------------------------------

def swap(list, p, q) {
    lp = list[p]; list[p] = list[q]; list[q] = lp
}

list = [1,2,3,4,5,6]
swap(list, 0, 3)
assert [4,2,3,1,5,6] == list


/*
 * Partitioning
 */
IntRange.metaClass.random = { ->
    from + new Random().nextInt(to - from + 1)
}

/* randomized partition to get Θ(log n) expected time */
def partition(list, left, right) {
    partition(list, left, right, (left..right).random())
}

def partition(list, left, right, pivotIndex) {
    pivot = list[pivotIndex]
    swap(list, pivotIndex, right) // Move pivot to end
    storeIndex = left
    (left..right-1).each {
        if (list[it] < pivot) swap(list, storeIndex++, it)
    }
    swap(list, right, storeIndex) // Move pivot to its final place
    storeIndex
}

list = [6,1,5,3,4,2]
assert 3 == partition(list, 0, list.size()-1, 4)
assert [1,3,2,4,5,6] == list



/*
 * http://en.wikipedia.org/wiki/Quicksort
 */
def quicksort(list) {
    quicksort(list, 0, list.size()-1)
}

def quicksort(list, p, q) {
    if (p < q) {
        r = partition(list, p, q)
        quicksort(list, p, r-1)
        quicksort(list, r+1, q)
    }
    list
}

assert [1,2,3,4,5,6,7,8] == quicksort([6,1,5,3,8,7,4,2])
assert [1,2,3,4,5,6,7,8] == quicksort([1,2,3,4,5,6,7,8])



/*
 * http://en.wikipedia.org/wiki/Merge_sort
 */
def mergeSort(list) {
    if (list.size() <= 1) return list
    def mean = list.size().intdiv(2)
    def left = list[0..mean-1]
    def right = list[mean..-1]
    merge(mergeSort(left), mergeSort(right))
}

def merge(left, right) {
    def result = []
    while (left.size() > 0 && right.size() > 0) {
        if (left[0] < right[0]) result << left.remove(0)
        else result << right.remove(0)
    }
    if (left.size() > 0) return result + left
    else return result + right
}

assert [1,2,3,4,5,6,7,8] == mergeSort([6,1,5,3,8,7,4,2])



/*
 * Calculates number of inversions in Θ(n log n) worst-case time.
 * CLRS, Problem 2-4.d, p. 42.
 * Modified merge sort with sentinels.
 */
def inversionCount(list) {
    inversionCount(list, 0, list.size() - 1)
}

def inversionCount(list, p, r) {
    int result = 0
    if (p < r) {
        def q = (p + r).intdiv(2)
        result += inversionCount(list, p, q)
        result += inversionCount(list, q + 1, r)
        result += inversionMerge(list, p, q, r)
    }
    result
}

def inversionMerge(list, p, q, r) {
    def left = cloneList(list, p, q) + [Integer.MAX_VALUE]
    def right = cloneList(list, q + 1, r) + [Integer.MAX_VALUE]
    int result = i = j = 0
    for (int k in p..r) {
        if (left[i] <= right[j]) {
            list[k] = left[i++]
        } else {
            list[k] = right[j++]
            result += left.size() - i - 1
        }
    }
    result
}

def cloneList(list, p, r) {
    def clone = []
    (p..r).each { clone << list[it] }
    clone
}

assert 5 == inversionCount([2,3,8,6,1])
assert 10 == inversionCount([5,4,3,2,1]) // n(n-1)/2



/*
 * http://en.wikipedia.org/wiki/Heapsort
 * http://datastructurefaqs.blogspot.com/2009/01/sorting-techniques-with-algorithm.html
 */
def heapsort(list) {
    heapsort(list, list.size())
}

def heapsort(list, count) {
    heapify(list, count)
    def end = count - 1
    while (0 < end) {
        swap(list, 0, end)
        siftDown(list, 0, end-1)
        end = end - 1
    }
    list
}

/* O(n) running time */
def heapify(list, count) {
    def start = (count - 2).intdiv(2)
    while (0 <= start) {
        siftDown(list, start, count-1)
        start = start - 1
    }
}

/* No recursion. CLRS, p.156, 6.2-5. O(log n) running time */
def siftDown(list, start, end) {
    def root = start
    while (root*2 + 1 <= end) {
        def child = root*2 + 1
        if (child < end && list[child] < list[child+1]) child = child + 1
        if (list[root] < list[child]) {
            swap(list, root, child)
            root = child
        } else return list
    }
}

assert [1,2,3,4,5,6,7,8] == heapsort([6,1,5,3,8,7,4,2])



/*
 * http://en.wikipedia.org/wiki/Insertion_sort
 * CLRS, p. 26. Worst case Θ(n2)
 */
def insertionSort(list) {
    for (int i in 1..<list.size()) {
        def key = list[i]
        int j = i - 1
        while (0 <= j && key < list[j]) {
            list[j + 1] = list[j]
            j = j - 1
        }
        list[j + 1] = key
    }
    list
}

assert [1,2,3,4,5,6,7,8] == insertionSort([6,1,5,3,8,7,4,2])



/*
 * http://en.wikipedia.org/wiki/Selection_sort
 */
def selectionSort(list) {
    for (int i in 0..<list.size()) {
        int index = i
        def min = list[i]
        for (int j in i..<list.size()) {
            if (list[j] < min) {
                min = list[j]
                index = j
            }
        }
        swap(list, i, index)
    }
    list
}

assert [1,2,3,4,5,6,7,8] == selectionSort([6,1,5,3,8,7,4,2])



/*
 * http://en.wikipedia.org/wiki/Selection_algorithm
 * Random select. O(n) expected running time. O(n2) worst case
 */
def select(list, p, q, i) {
    if (p == q) return list[p]
    r = partition(list, p, q)
    k = r - p + 1
    if (i == k) return list[r]
    if (i < k) return select(list, p, r-1, i)
    select(list, r+1, q, i-k)
}

def select(list, i) {
    select(list, 0, list.size()-1, i)
}

def min(list) {
    select(list, 1)
}

def max(list) {
    select(list, list.size())
}

def median(list) {
    select(list, list.size()/2)
}

assert 3 == select([6,1,5,3,8,7,4,2], 3)
assert 4 == select([6,1,2,3,8,4,4,2], 5)

assert 1 == min([6,1,5,3,8,7,4,2])
assert 8 == max([6,1,5,3,8,7,4,2])
assert 4 == median([6,1,5,3,8,7,4,2])



// ------------------------------------------------------------------
// Outside of comparison sort algorithms
// http://www.catonmat.net/blog/mit-introduction-to-algorithms-part-three
// ------------------------------------------------------------------

/*
 * http://en.wikipedia.org/wiki/Counting_sort
 */
def countingSort(list) {
    min = list.min(); max = list.max()
    range = max - min
    counts = new int[range+1]
    list.each { counts[it-min]++ }
    (1..counts.size()-1).each { counts[it] += counts[it-1] }

    result = new int[list.size()]
    (list.size()-1..0).each {
        index = list[it]
        result[counts[index-min]-- -1] = index
    }
    result
}

assert [3,4,4,5,6,7] == countingSort([7,3,6,4,5,4])



/*
 * http://en.wikipedia.org/wiki/Radix_sort
 */
def radixSort(list) {
    int passes = maxItemLength(list)
    def result = list
    (1..passes).each { digit ->
        result = mergeBuckets(splitToBuckets(result, digit))
    }
    result
}

int maxItemLength(list) {
    def max = Integer.MIN_VALUE
    list.each { if (max < it) max = it }
    Math.ceil(Math.log10(max+1)) as int
}

def splitToBuckets(list, k) {
    def buckets = emptyBuckets(10)
    list.each { buckets[getDigit(it, k)] << it }
    buckets
}

def emptyBuckets(k) {
    def buckets = new LinkedList[10]
    (0..<k).each { buckets[it] = new LinkedList() }
    buckets
}

int getDigit(number, k) {
    number.intdiv(10**(k-1)).mod(10)
}

def mergeBuckets(buckets) {
    def result = []
    buckets.each { result += it }
    result
}

assert [2,24,45,66,75,90,170,802] == radixSort([170,45,75,90,2,24,802,66])
