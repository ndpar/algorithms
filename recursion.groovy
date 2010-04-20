// http://en.wikipedia.org/wiki/Counting_sort

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


def swap(list, p, q) {
    lp = list[p]; list[p] = list[q]; list[q] = lp
}

list = [1,2,3,4,5,6]
swap(list, 0, 3)
assert [4,2,3,1,5,6] == list


// http://en.wikipedia.org/wiki/Selection_algorithm

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


def select(list, p, q, i) {
    if (p == q) return list[p]
    pivotIndex = p + new Random().nextInt(q-p+1) // randomized partition to get O(n)
    r = partition(list, p, q, pivotIndex)
    k = r - p + 1
    if (i == k) return list[r]
    if (i < k) return select(list, p, r-1, i)
    select(list, r+1, q, i-k)
}


list = [6,1,5,3,8,7,4,2]
assert 4 == select(list, 0, list.size()-1, 4)

list = [6,1,5,3,8,7,4,2]
assert 1 == select(list, 0, list.size()-1, 1) // min

list = [6,1,5,3,8,7,4,2]
assert 8 == select(list, 0, list.size()-1, list.size()) // max

list = [6,1,2,3,8,4,4,2]
assert 4 == select(list, 0, list.size()-1, 5)
