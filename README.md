# CCLV

A Compiler of a Tiny Procedural Programming Language for the [Fenestra6502](https://github.com/hatsugai/Fenestra6502) system.

NOTE: Only functions needed for evaluating the performance of Fenestra6502 are implemented.

## Examples

### fib

```c
fib(n)
{
    if (n < 2)
        return n;
    else
        return fib(n - 1) + fib(n - 2);
}
```

### tarai

```c
tarai(x, y, z)
{
    if (x <= y)
        return y;
    else
        return tarai(tarai(x - 1, y, z),
                     tarai(y - 1, z, x),
                     tarai(z - 1, x, y));
}
```
