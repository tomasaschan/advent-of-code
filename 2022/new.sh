#!/bin/bash

day=${1?"usage: $0 <day>"}

mkdir -p src/dec$day
cat > src/dec$day/solve.py <<EOF
#!/usr/bin/env python3

import sys

input = sys.stdin.read()


def a():
    pass


def b():
    pass


print("a:", a())
print("b:", b())
EOF

chmod +x src/dec$day/solve.py
touch src/dec$day/input.txt
touch src/dec$day/example.txt
