#!/opt/homebrew/bin/python3

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
        result = subprocess.run(f"{command} < {filename}", shell=True, capture_output=True, text=True)
        if result.stdout:
            return result.returncode, result.stdout.split()[-1]
        else:
            return result.returncode, ""
    except Exception as e:
        print(f"Error running {command}: {e}")
        sys.exit(1)


def run_chez_scheme(filename):
    """Runs Chez Scheme interpreter and returns the output."""
    try:
        chez_output = subprocess.run(
            ["chez", "--quiet"],
            input=open(filename).read(),
            capture_output=True,
            text=True
        )
        return chez_output.returncode, chez_output.stdout.strip()
    except Exception as e:
        print(f"Error running Chez Scheme: {e}")
        sys.exit(1)

def main():
    if len(sys.argv) != 2:
        print("Usage: python compare.py <file-directory>")
        sys.exit(1)

    dir_path = sys.argv[1]
    exe_path = ".stack-work/dist/aarch64-osx/ghc-9.6.6/build/my-lisp-interpreter-exe/my-lisp-interpreter-exe"

    if not os.path.exists(exe_path):
        print("Please build the project first.")
        sys.exit(1)

    lisp_interpreter = exe_path

    time_lisp = 0
    time_chez = 0

    files = os.listdir(f"./{dir_path}")
    file_length = len(files)

    for i, file in enumerate(files):
        file_path = dir_path + "/" + file
        file_path = os.path.realpath(file_path)
        start = time.time()
        lisp_code, lisp_output = run_interpreter(f"{lisp_interpreter}", file_path)
        end = time.time()
        time_lisp += end - start
        start = time.time()
        chez_code, chez_output = run_chez_scheme(file_path)
        end = time.time()
        time_chez += end - start
        if lisp_code == chez_code and lisp_output == chez_output:
            print(style.GREEN + f"{i + 1}/{file_length} ", end="")
            print(f"{file} passed")
        else:
            print(style.RED + f"{i + 1}/{file_length} ", end="")
            print(f"{file} failed")
        print(style.RESET)
    print(f"Total time for {lisp_interpreter.split('/')[-1]}: {time_lisp}")
    print(f"Total time for Chez Scheme: {time_chez}")

if __name__ == "__main__":
    main()
