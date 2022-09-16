# words
hash mapped wordlist in fortran

allows for speedy lookups. permutate function found on comp.lang.fortran

## generate your own wordlist
aspell -d en dump master | aspell -l en expand | sed "s/\w*'//g;s/ \+/\n/g" | awk '{ print tolower($0) }' | uniq > wordlist

you may need to run uniq twice to remove duplicates else the wordlist cannot be mapped
