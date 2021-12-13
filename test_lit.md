Construct a table

```futhark
let e = tabulate_2d 64 64 (\i j -> (f32.i64 (i * j)) / (f32.i64 (64 * 64)))
```



![](test_lit-img/65dc228d59ce0363ffcf4dbc5f3b408a-img.png)


Now show a value

```futhark
let f = iota 5
```

```
> f
```

```
[0i64, 1i64, 2i64, 3i64, 4i64]
```

