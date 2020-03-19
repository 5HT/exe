# HTS: Homotopy Type System for Erlang

[![Build Status](https://travis-ci.com/groupoid/hts.svg?branch=master)](https://travis-ci.com/groupoid/hts)
[![Hex pm](http://img.shields.io/hexpm/v/hts.svg?style=flat)](https://hex.pm/packages/hts)

```
$ ./homotopy
HTS CTT-CCHM Homotopy Type System 1.3.1

 usage = homotopy args
  args = [] | cmd | cmd args
   cmd = parse <tokens> | lex <string> | read <name>
       | fst <tuple> | snd <tuple>
       | a <name> | file <name>
```

# Erlang

```sh
$ sudo apt install erlang
```
```sh
$ curl -fsSL https://git.io/fpYm4 > mad
$ chmod +x mad
$ sudo cp mad /usr/local/bin
```
```sh
$ mad compile release homotopy
$ ./homotopy parse file priv/mltt.ctt
```

# Elixir

```elixir
$ sudo apt install elixir
```
```elixir
$ mix deps.get
$ iex -S mix
> :homotopy.console ['parse','file','priv/mltt.ctt']
```

# Credits

Namdak Tonpa
