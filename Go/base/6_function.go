package main

func f1(a int, b int) int {
    return 1
}

func f2(a, b int) int {
    return 1
}

func f3(a int, b int) (int, bool) {
    return 1, true
}

func main() {
    var a int = 1
    var b bool = true

    // 函数
    a = f1(a, 1)
    a, b = f3(1, 2)

    // 匿名函数
    func() {
        var c int = 1
    }()

    func() int {
        var c int = 1
        return c
    }()

    var anon_f = func() {}
    anon_f()
}
