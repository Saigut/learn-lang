package main

func main() {
    var a, b, c int = 1, 1, 1

    // if
    if a < 0 {
        a = b
    }

    if a < 0 {
        a = b
    } else {
        a = c
    }

    // switch case
    switch a {
    case 1:
        a = b
    case 2:
        a = b
    case 3, 4, 5:
        a = b
    default:
        a = b
    }

    switch {
    case a == 1:
        a = b
    case a == 2:
        a = b
    default:
        a = b
    }

    // goto
    a = b
    goto label
label:
    a = c
}
