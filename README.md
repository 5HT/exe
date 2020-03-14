# HTS: Homotopy Type System for Erlang

# Prerequisites

HTS is written in Erlang and uses <a href="https://mad.n2o.dev">mad</a> Erlang build tool.

```sh
$ sudo apt install erlang
$ curl -fsSL https://git.io/fpYm4 > mad
$ chmod +x mad
$ sudo cp mad /usr/local/bin
```

# Build

```sh
$ mad dep com pla release cub
$ ./cub a priv/mltt.ctt
```

# Credits

Namdak Tonpa
