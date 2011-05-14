/**
 * The Skyline Problem
 * http://online-judge.uva.es/p/v1/105.html
 *
 * Sweep Line Algorithm
 * http://www.algorithmist.com/index.php/UVa_105
 */
import static Operation.*

@Grab(group='com.google.guava', module='guava', version='r05')
import com.google.common.collect.TreeMultiset as Tree

/*
 * To implement the algorithm we need two data structures:
 * - min-heap to store insert-remove events, and
 * - balanced binary search tree to store building heights
 */

class MaxIntegerList {
    private Tree<Integer> list = new Tree<Integer>()

    boolean add(Integer i) {
        list << i
    }
    Integer max() {
        list.elementSet() ? list.elementSet().last() : null
    }
    def remove(Integer i) {
        list.remove i
    }
}

enum Operation {
    INSERT, REMOVE
}

class Event {
    Integer coordinate
    Integer height
    Operation operation
}

class MinEventHeap {
    private PriorityQueue<Event> queue

    MinEventHeap(int initialCapacity) {
        queue = new PriorityQueue<Event>(initialCapacity, new EventComporator())
    }
    boolean insert(Event i) {
        queue.add i
    }
    Event extractMin() {
        queue.poll()
    }
    int size() {
        queue.size()
    }
    static class EventComporator implements Comparator<Event> {
        int compare(Event p, Event q) {
            if (p.coordinate != q.coordinate) return p.coordinate.compareTo(q.coordinate)
            if (p.operation == INSERT && q.operation == INSERT) return q.height.compareTo(p.height)
            if (p.operation == REMOVE && q.operation == REMOVE) return p.height.compareTo(q.height)
            p.operation == INSERT ? -1 : 1
        }
        boolean equals(Object o) {
            return o instanceof EventComporator
        }
    }
}

/*
 * Main algorithm
 */
def skyline(buildings) {
    processEvents(buildEventHeap(buildings))
}

def buildEventHeap(buildings) {
    def result = new MinEventHeap(2 * buildings.size())
    buildings.each { // it = [x1, height, x2]
        result.insert(new Event(coordinate: it[0], height: it[1], operation: INSERT))
        result.insert(new Event(coordinate: it[2], height: it[1], operation: REMOVE))
    }
    result
}

def processEvents(eventHeap) {
    def heights = new MaxIntegerList()
    def result = []
    int x = Integer.MIN_VALUE, height = 0
    while ((event = eventHeap.extractMin()) != null) {
        // 1. Process event
        event.operation == INSERT ? heights.add(event.height) : heights.remove(event.height)
        // 2. Get maximum height
        def h = heights.max() ?: 0
        // 3. Add new output on height change
        if (height != h) {
            x = event.coordinate
            height = h
            result << x << height
        }
    }
    result
}


assert [2,4,8,0] == skyline([[2,4,5],[4,4,8]])
assert [2,4,8,0] == skyline([[2,4,5],[5,4,8]])
assert [2,4,6,0] == skyline([[2,4,6],[2,2,6]])

assert [1,11,3,13,9,0,12,7,16,3,19,18,22,3,23,13,29,0] ==
    skyline([[1,11,5],[2,6,7],[3,13,9],[12,7,16],[14,3,25],[19,18,22],[23,13,29],[24,4,28]])
