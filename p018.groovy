// inspired by
// http://blog.dreamshire.com/2009/04/01/project-euler-problem-18-solution
// See also http://en.wikipedia.org/wiki/Dynamic_programming

// to solve problem 67 replace file name to triangle67.txt

def max(x, y) {
    x < y ? y : x
}

triangle = []
new File('triangle18.txt').eachLine { line ->
    triangle << line.split(/ /)*.toInteger()
}
(triangle.size()-1).downto(1) { i ->
    0.upto(i-1) { j ->
        triangle[i-1][j] += max(triangle[i][j], triangle[i][j+1])
    }
    
}
println triangle[0][0]