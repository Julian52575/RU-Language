#!/bin/python3

import subprocess
import sys
import os
import time

class style():
    RED = '\033[31m'
    GREEN = '\033[32m'
    RESET = '\033[0m'

class Error():
    def __init__(self, filename, list_output, chez_output):
        self.filename = filename
        self.list_output = list_output
        self.chez_output = chez_output

    def __str__(self):
        return f"--------------------\nError in {self.filename}\nList output:\n{self.list_output}\nChez output:\n{self.chez_output}\n--------------------"

class Test():
    def __init__(self, interpretor, dir_path):
        self.interpretor = interpretor
        self.returncode = 0
        self.time_lisp = 0
        self.time_chez = 0
        self.total_files = self.get_total_files(dir_path)
        self.file_index = 0
        self.file_errors = []

    def get_total_files(self, dir_path):
        files = os.listdir(f"{dir_path}")
        total_files = 0
        for file in files:
            full_path = os.path.join(dir_path, file)
            real_path = os.path.realpath(full_path)
            if os.path.isdir(real_path):
                total_files += self.get_total_files(real_path)
            else:
                total_files += 1
        return total_files

def run_interpreter(command, filename):
    """Runs an interpreter command and returns the output."""
    try:
        result = subprocess.run(f"{command} < {filename}", shell=True, capture_output=True, text=True)
        if result.stdout == "":
            return result.returncode, "", result
        return result.returncode, result.stdout.split()[-1], result
    except Exception as e:
        print(f"Error running {command}: {e}")
        sys.exit(1)

def run_chez_scheme(filename):
    """Runs Chez Scheme interpreter and returns the output."""
    try:
        chez_output = subprocess.run(
            ["scheme", "--quiet"],
            input=open(filename).read(),
            capture_output=True,
            text=True
        )
        if (chez_output.stderr.__contains__("Exception")):
            return 84, chez_output.stderr.strip(), chez_output
        return chez_output.returncode, chez_output.stdout.strip(), chez_output
    except Exception as e:
        print(f"Error running Chez Scheme: {e}")
        sys.exit(1)

def execute_dir(test: Test, dir_path):
    files = os.listdir(f"./{dir_path}")

    for file in files:
        full_path = os.path.join(dir_path, file)
        real_path = os.path.realpath(full_path)
        if os.path.isdir(real_path):
            execute_dir(test, os.path.join(dir_path, file))
            continue
        start = time.time()
        lisp_code, lisp_output, lisp_res = run_interpreter(f"./{test.interpretor}", real_path)
        end = time.time()
        test.time_lisp += end - start
        start = time.time()
        chez_code, chez_output, chez_res = run_chez_scheme(real_path)
        end = time.time()
        test.time_chez += end - start 
        if (lisp_code == 84 and chez_code == 84) or (lisp_code == chez_code and lisp_output == chez_output):
            print(style.GREEN + f"{test.file_index + 1}/{test.total_files} ", end="")
            print(f"{file} passed", end="")
        else:
            print(style.RED + f"{test.file_index + 1}/{test.total_files} ", end="")
            print(f"{file} failed", end="")
            test.file_errors.append(Error(file, lisp_res.stderr, chez_res.stdout))
            test.returncode = 1
        test.file_index += 1
        print(style.RESET)
    return test

def main():
    if len(sys.argv) != 2:
        print("Usage: python compare.py <file-directory>")
        sys.exit(1)

    dir_path = sys.argv[1]
    exe_path = ".stack-work/dist/x86_64-linux-tinfo6/ghc-9.6.6/build/my-lisp-interpreter-exe/my-lisp-interpreter-exe"

    if not os.path.exists(exe_path):
        print("Please build the project first.")
        sys.exit(1)

    test = Test(exe_path, dir_path)
    execute_dir(test, dir_path)
    print(f"\nTotal time for {test.interpretor.split('/')[-1]}: {test.time_lisp}")
    print(f"Total time for Chez Scheme: {test.time_chez}")

    if (len(test.file_errors) > 0):
        with open("errors.txt", "w") as f:
            for error in test.file_errors:
                f.write(str(error))
                f.write("\n")
    sys.exit(test.returncode)

if __name__ == "__main__":
    main()
