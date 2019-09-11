#!/bin/bash

ormolu --mode inplace $(find app -type f -name "*.hs")