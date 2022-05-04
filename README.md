# purescript-frp-examples

This repo has several examples of small FRP apps written in PureScript.

## Motivation

There are more and more folks interested in using [`purescript-event`](https://github.com/mikesol/purescript-event) and the various libraries that derive from it, such as [`purescript-behaviors`](https://github.com/mikesol/purescript-behaviors), [`purescript-deku`](https://github.com/mikesol/purescript-deku) and [`purescript-wags`](https://github.com/mikesol/purescript-wags). Without a basic understanding of functional reactive programming, using these libraries can be challenging. This repo seeks to provide an introduction to FRP through examples written in `deku` and `wags`. Each example aims to be small (less than 300 loc including imports) while non-trivial (except for the hello world).

## FRP in PureScript

There are many different dialects and flavors of FRP, and `purescript-event` is by no means the only one. [`purescript-signal`](https://github.com/bodil/purescript-signal) is another great FRP library, and UI frameworks like [`purescript-concur`](https://github.com/purescript-concur/purescript-concur-react) use many concepts from FRP. By working through these examples, you'll hopefully build useful intuition for working with all of these libraries.