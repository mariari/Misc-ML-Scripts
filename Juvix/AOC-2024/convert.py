#!/bin/env python

import sys

file_name = sys.argv[1]

with open(file_name, "r") as file:
    content = file.read()

ascii_list = [ord(char) for char in content]

print("[" + ";".join(map(str, ascii_list)) + "]")
