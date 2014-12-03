/**
 * The Skyline Problem
 * http://online-judge.uva.es/p/v1/105.html
 *
 * Sweep Line Algorithm
 * http://www.algorithmist.com/index.php/UVa_105
 *
 * To implement the algorithm we need two data structures:
 * - min-heap to store insert-remove events, and
 * - max-heap to store building heights.
 *
 * With these data structures the running time of the algorithm is O(n log n).
 */
import static Operation.*

enum Operation {
    INSERT, REMOVE
}

class Event {
    Integer coordinate
    Integer height
    Operation operation
}

/*
 * The core of the algorithm is to sort all events properly.
 * With sorted events, the rest of the algorithm is pretty simple.
 */
class EventComporator implements Comparator<Event> {
    int compare(Event p, Event q) {
        if (p.coordinate != q.coordinate) return p.coordinate.compareTo(q.coordinate)
        if (p.operation == INSERT && q.operation == INSERT) return q.height.compareTo(p.height)
        if (p.operation == REMOVE && q.operation == REMOVE) return p.height.compareTo(q.height)
        p.operation == INSERT ? -1 : 1
    }
}

/*
 * Main algorithm.
 */
def skyline(buildings) {
    processEvents(buildEventHeap(buildings))
}

/*
 * Insert 2n entries into min-heap.
 * Running time is O(n log n)
 */
def buildEventHeap(buildings) {
    def result = new PriorityQueue<Event>(2 * buildings.size(), new EventComporator())
    buildings.each { // it = [left_coordinate, height, right_coordinate]
        result << new Event(coordinate: it[0], height: it[1], operation: INSERT)
        result << new Event(coordinate: it[2], height: it[1], operation: REMOVE)
    }
    result
}

/*
 * Loop through the event heap and check if the height is being changed.
 * If so, add event's coordinate and height to the result list.
 * Running time is O(n log n).
 */
def processEvents(eventHeap) {
    def result = []
    def heights = new PriorityQueue<Integer>(eventHeap.size(), {n,m -> m.compareTo(n)} as Comparator)
    int lastMaxHeight = 0
    while ((event = eventHeap.poll()) != null) { // 2n * O(log n)
        event.operation == INSERT ? heights.add(event.height) : heights.remove(event.height) // O(log n)
        int currentMaxHeight = heights.peek() ?: 0 // O(1)
        if (lastMaxHeight != currentMaxHeight) {
            result << event.coordinate << currentMaxHeight
            lastMaxHeight = currentMaxHeight
        }
    }
    result
}


assert [2,4,8,0] == skyline([[2,4,5],[4,4,8]])
assert [2,4,8,0] == skyline([[2,4,5],[5,4,8]])
assert [2,4,6,0] == skyline([[2,4,6],[2,2,6]])

assert [1,11,3,13,9,0,12,7,16,3,19,18,22,3,23,13,29,0] ==
    skyline([[1,11,5],[2,6,7],[3,13,9],[12,7,16],[14,3,25],[19,18,22],[23,13,29],[24,4,28]])
