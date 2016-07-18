#!/usr/bin/env bash

set -e

git checkout -b gh-pages
./build.sh
git add -f target/
git add index.html
git commit -m"Update with latest code."
git push -f origin gh-pages
git checkout master
git branch -D gh-pages
