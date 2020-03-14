# HTS: Homotopy Type System for Erlang

[![Build Status](https://travis-ci.com/groupoid/hts.svg?branch=master)](https://travis-ci.com/groupoid/hts)
[![Hex pm](http://img.shields.io/hexpm/v/hts.svg?style=flat)](https://hex.pm/packages/hts)


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
