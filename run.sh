#!/bin/sh

mkdir -p out
scalac -d out Complex.scala Quantum.scala
scala -cp out -Yrepl-sync -i boot.scala
