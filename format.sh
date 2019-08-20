#!/bin/bash

while IFS= read -r -d '' file
do
  ormolu "$file" --mode inplace
done < <(find app/ -type f -name "*hs" -not -path '.git' -not -path '*.dist-newstyle' -not -path '*.stack-work*' -print0)