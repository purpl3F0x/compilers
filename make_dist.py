#!usr/bin/python

import os
import shutil
import sys

def make_dist(path='./dist'):
    # Create the dist directory
    if not os.path.exists(path):
        os.makedirs(path)

    # Copy the files to the dist directory
    shutil.copy("./target/release/alanc", path)
    shutil.copy("./target/release/libalan.a",path)


if __name__ == '__main__':
    argc = len(sys.argv)
    if(argc>= 2):
        print(f"Usage: python {sys.argv[0]} <path>?")
        sys.exit(1)

    elif argc == 2:
        make_dist(sys.argv[1])

    else:
        make_dist()
