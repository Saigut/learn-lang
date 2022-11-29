package main

func main() {
    // 整形
    var a int = 1 // 空间大小要看平台
    var a int8 = 1
    var a uint8 = 1
    var a uint64 = 1
    var a uint8 = 0x12 // hexadecimal literal
    var a uint8 = 0o12 // octal literal
    var a uint8 = 0b01 // binary literal

    // 浮点数
    var a float32 = 1.1
    var a float32 = 1.1e+10
    var a float32 = 1.1e-10
    var a float64 = 1.1e-10

    // 字符
    var a byte = 'a'
    var b = 'a' // rune type。用于支持多字节的字符

    // 字符串
    var a string = "aa"

    // 布尔
    var a bool = true

    // 字节
    var a byte = 97

    // 数组
    var a []int
    var a [2]int
    var a = [2]int{1, 1} // 这语法搁着杂交呢
    var a = [3]int{1: 1, 2: 1}
    a[0]
    a[1:2]
    a[:2]
    a[1:]
    a[:]

    // 类型转换
    var a uint8 = 1
    var b int = a // 初始化可隐式转换。一会能隐式一会不能，辣鸡
    b = int32(a)  // 赋值必须显式转换
    var f float32 = float32(a)

    // string -> []byte
    var b = []byte("aa")
    var b1 []byte
    b1 = []byte("aa")

    // []byte -> string
    var db0 = []byte{65, 66}
    var s = string([]byte{65, 66})
    s = string([]byte{65, 66})
    s = string(db0)

    // []byte -> [32]byte
    var fb [32]byte
    var db0 []byte
    var db = []byte{65, 66}
    // 看这两行类型转换，傻逼一样。值和指针分不清？
    fb = *((*[32]byte)(db))
    db0 = *((*[]byte)(&db))

    // [32]byte -> []byte
    var db1 []byte = fb[:]
    db1 = fb[0:2]
}
