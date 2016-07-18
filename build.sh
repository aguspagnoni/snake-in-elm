#!/usr/bin/env bash

set -e

elm make --output target/elm.js main.elm
elm-make main.elm --output=index.html
