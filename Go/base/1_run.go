package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}

// 设置 go 下载库存放位置
// go env -w GOPATH=X:\xxxx
// go env -w GOPATH=/xxx/xxx
// 另开终端，检查：
// go env GOPATH
// go env GOMODCACHE

// * 编译：go build .\1_run.ss.go。再运行
// * 入口函数 main 必须在 main package 里。
// * 同一个目录下的文件 package 名字相同。package 名字可以与目录名字相同，也可以不同

// * 例子
// 结构：
// proj_dir/
//   src/
//     code.go
// cd proj_dir && go mod init proj_name
// $ go build ./src/code.go
