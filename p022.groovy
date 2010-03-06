def wordValue(word) {
    word.bytes.inject(0) { sum, i -> sum + i - 64 }
}

names = []
new File('names.txt').eachLine { line ->
    line.split(/,/).each { names << it[1..-2] }
}

result = 0
names.sort().eachWithIndex { name, i -> result += (i + 1) * wordValue(name) }

println result