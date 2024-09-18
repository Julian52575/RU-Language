#!/bin/python3

import subprocess
import sys
import os
import time

class style():
    RED = '\033[31m'
    GREEN = '\033[32m'
    RESET = '\033[0m'

def run_interpreter(command, filename):
    """Runs an interpreter command and returns the output."""
    try:
        result = subprocess.run([command, filename], capture_output=True, text=True)
        return result.stdout.strip()
    except Exception as e:
        print(f"Error running {command}: {e}")
        sys.exit(1)

def run_chez_scheme(filename):
    """Runs Chez Scheme interpreter and returns the output."""
    try:
        chez_output = subprocess.run(
            ["chez-scheme", "--quiet"],
            input=open(filename).read(),
            capture_output=True,
            text=True
        ).stdout.strip()
        return chez_output
    except Exception as e:
        print(f"Error running Chez Scheme: {e}")
        sys.exit(1)

def main():
    if len(sys.argv) != 3:
        print("Usage: python compare.py <lisp-interpreter> <file-directory")
        sys.exit(1)

    lisp_interpreter = sys.argv[1]
    dir_path = sys.argv[2]

    time_lisp = 0
    time_chez = 0

    files = os.listdir(f".{dir_path}")
    file_length = len(files)

    for i, file in enumerate(files):
        start = time.time()
        lisp_output = run_interpreter(f"./{lisp_interpreter}", file)
        end = time.time()
        time_lisp += end - start
        start = time.time()
        chez_output = run_chez_scheme(file)
        end = time.time()
        time_chez += end - start
        print(f"{i + 1}/{file_length}", end="")
        if lisp_output != chez_output:
            print(style.RED + f"{file} failed")
        else:
            print(style.GREEN + f"{file} passed")
        print(style.RESET)
    print(f"Total time for {lisp_interpreter}: {time_lisp}")
    print(f"Total time for Chez Scheme: {time_chez}")

if __name__ == "__main__":
    main()
