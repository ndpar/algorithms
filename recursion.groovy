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
 * http://en.wikipedia.org/wiki/Heapsort
 * http://datastructurefaqs.blogspot.com/2009/01/sorting-techniques-with-algorithm.html
 * No recursion actually
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

def heapify(list, count) {
    def start = (count - 2).intdiv(2)
    while (0 <= start) {
        siftDown(list, start, count-1)
        start = start - 1
    }
}

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
 */
def insertionSort(list) {
    for (int i in 1..<list.size()) {
        int c = i
        while (0 < c) {
            if (list[c] < list[c-1]) {
                swap(list, c-1, c)
                c = c - 1
            } else break
        }
    }
    list
}

assert [1,2,3,4,5,6,7,8] == insertionSort([6,1,5,3,8,7,4,2])



/*
 * http://en.wikipedia.org/wiki/Selection_algorithm
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