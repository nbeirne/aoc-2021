package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "strconv"
)


func main() {
    fmt.Println("hello world")

    content, _ := ioutil.ReadFile("test")

    strbuffer := string(content)

    // parse
    sArr := strings.Split(strbuffer, ",")
    var arr = []int{}
    for _, i := range sArr {
        j, _ := strconv.Atoi(i)
        arr = append(arr, j)
    }

    // solve

    values := make([]int, 2)
    for n, _ := range values {
        fmt.Println(n)
    }

    fmt.Println(arr)
}
