#!/usr/bin/env bash
stack test --fast --haddock-deps && stack hoogle -- generate --local && stack hoogle -- server --local --port=8888