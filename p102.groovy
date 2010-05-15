/*
 * Three distinct points are plotted at random on a Cartesian plane,
 * for which -1000 <= x, y <= 1000, such that a triangle is formed.
 *
 * Consider the following two triangles:
 *
 * A(-340,495), B(-153,-910), C(835,-947)
 * X(-175,41), Y(-421,-714), Z(574,-645)
 *
 * It can be verified that triangle ABC contains the origin, whereas
 * triangle XYZ does not.
 *
 * Using triangles.txt, a 27K text file containing the co-ordinates
 * of one thousand "random" triangles, find the number of triangles
 * for which the interior contains the origin.
 */

boolean insideTriangle(t, x) {
    insideTriangle(t[0,1], t[2,3], t[4,5], x)
}

boolean insideTriangle(a, b, c, x) {
    onSemiPlane(a, b, c, x) && onSemiPlane(b, c, a, x) && onSemiPlane(c, a, b, x)
}

boolean onSemiPlane(a, b, c, d) {
    onLine(a, b, c) * onLine(a, b, d) >= 0
}

long onLine(a, b, c) {
    (c[0] - a[0]) * (b[1] - a[1]) - (c[1] - a[1]) * (b[0] - a[0])
}

assert insideTriangle([-340,495],[-153,-910],[835,-947],[0,0])
assert !insideTriangle([-175,41,-421,-714,574,-645],[0,0])

int count = 0
new File('triangles.txt').eachLine {
    def triangle = it.split(/,/)*.toInteger()
    if (insideTriangle(triangle, [0,0])) count++
}
println count