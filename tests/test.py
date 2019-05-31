#!/usr/bin/python
# -*- coding: utf-8 -*-
# ************************************************************
#
#                       PolyOp
# 
# Université Paris 13, LIPN, CNRS, France
# 
# Script description: TESTATOR (script for non-regression tests)
# 
# File contributors : Étienne André
# Created           : 2012/05/??
# Last modified     : 2017/03/21
# Copied from IMITATOR: 2019/05/31
# Last modified: 2019/05/31
# ************************************************************


# ************************************************************
# MODULES
# ************************************************************
from __future__ import print_function

import datetime
import os
import subprocess
import sys
from collections import namedtuple

import re

# To output colored text
Colors = namedtuple('Colors', 'ERROR, BOLD, GOOD, NORMAL, WARNING')

bcolors = Colors(ERROR='\033[1;37;41m',
                 BOLD='\033[1m',
                 GOOD='\033[1;32;40m',
                 NORMAL='\033[0m',
                 WARNING='\033[93;40m')

# ************************************************************
# GENERAL CONFIGURATION
# ************************************************************

# Path to the tests directory
TEST_PATH = os.path.dirname(os.path.abspath(__file__))
# Root path to the main PolyOp root directory
POLYOP_PATH = os.path.dirname(TEST_PATH)
# Path to the example directory
EXAMPLE_PATH = os.path.join(TEST_PATH, 'testcases/')
# Path to the binary directory
BINARY_PATH = os.path.join(POLYOP_PATH, 'bin/')

# Name for the binary to test
BINARY_NAME = 'polyop'
# Log file for the binary
LOGFILE = os.path.join(TEST_PATH, 'tests.log')

# ************************************************************
# BY DEFAULT: ALL TO LOG FILE
# ************************************************************
orig_stdout = sys.stdout
logfile = open(LOGFILE, 'w')
sys.stdout = logfile


# ************************************************************
# FUNCTIONS
# ************************************************************
def make_binary(binary):
    return os.path.join(BINARY_PATH, binary)


def make_file(file_name):
    return os.path.join(EXAMPLE_PATH, file_name)


def fail_with(text):
    print_to_log('Fatal error!')
    print_to_log(text)
    sys.exit(1)


def print_warning(text):
    print_to_log(' *** Warning: %s' % text)


def print_error(text):
    print_to_log(' *** Error: %s' % text)


# Print text to log file
def print_to_log(content):
    print(content)


def print_to_screen(content):
    # Revert stdout
    sys.stdout = orig_stdout
    # Print
    print(content)
    # Put back stdout to log file
    sys.stdout = logfile


# Print text both to log file and to screen
# NOTE: can probably do better…
def print_to_screen_and_log(content):
    # Print to log
    print_to_log(content)
    # Also print to screen
    print_to_screen(content)


# WARNING: this function only removes non-nested comments! But in our tests and outputs, there are no nested comments…
def removeComments(string):
	# NOTE: from https://stackoverflow.com/questions/2319019/using-regex-to-remove-comments-from-source-files
	string = re.sub(re.compile("\(\*.*?\*\)",re.DOTALL ) ,"" ,string) # remove all occurrences streamed comments ((*COMMENT *)) from string
	return string


# ************************************************************
# FORMATS
# ************************************************************
header_benchmark = """

############################################################
 BENCHMARK {benchmark_id}
 purpose: {purpose}
"""

test_fmt = """\
  Test {expectation_id} failed!

*** Expected content for this test:

{expected_content}

*** Content found:

{original_content}

*** Reformatted expected content for this test:

{reformatted_expected_content}

*** Reformatted content found:

{reformatted_content_found}


"""


# ************************************************************
# MAIN TESTING FUNCTION
# ************************************************************

def test(binary_name, tests, logfile, logfile_name):
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # CHECK FOR THE EXISTENCE OF BINARIES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    binary = make_binary(binary_name)
    if not os.path.exists(binary):
        fail_with('Binary %s does not exist' % binary)

    print_to_screen('\n{c.BOLD}# START TESTING BINARY {name}{c.NORMAL}'.format(c=bcolors, name=binary_name))

    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # TEST CASES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # Id for benchmarks
    benchmark_id = 1
    # Number of passed benchmarks
    passed_benchmarks = 0

    # Id for test case
    test_case_id = 1
    # Number of passed test cases
    passed_test_cases = 0

    for test_case in tests:
        # Initially everything is ok
        passed = True

        # Print something
        print_to_log(header_benchmark.format(benchmark_id=benchmark_id,
                                             purpose=test_case['purpose']))
        print_to_screen(' Benchmark {}: {}..'.format(benchmark_id, test_case['purpose']))

        # Add the path to all input files
        # TODO: test for existence of files (just in case)
        cmd_inputs = [make_file(each_file) for each_file in test_case['input_files']]

        # ------------------------------------------------------------
        cmd = [binary] + cmd_inputs + (test_case['options']).split()

        # Print the command
        print_to_log(' command : ' + ' '.join(cmd))

        # Launch!
        # NOTE: flushing avoids to mix between results of the binary, and text printed by this script
        logfile.flush()
        subprocess.call(cmd, stdout=logfile, stderr=logfile)
        logfile.flush()

        # Files to remove
        files_to_remove = set()

        # Check the expectations
        for expectation_id, expectation in enumerate(test_case['expectations']):
            # Build file
            output_file = make_file(expectation['file'])

            test_expectation_id = '{}.{}'.format(benchmark_id, expectation_id)

            # Check existence of the output file
            if not os.path.exists(output_file):
                print_to_log(' File {} does not exist! Test {} failed.'.format(output_file, test_expectation_id))
                passed = False
            else:
                # Read file
                with open(output_file, "r") as my_file:
                    # Get the content
                    original_content = my_file.read()
                    # Replace all whitespace characters (space, tab, newline, and so on) with a single space and remove comments
                    content = ' '.join((removeComments(original_content)).split())

                    # Replace all whitespace characters (space, tab, newline, and so on) with a single space
                    expected_content = ' '.join((removeComments(expectation['content'])).split())

                    # Look for the expected content
                    position = content.find(expected_content)

                    if position >= 0:
                        print_to_log(' Test %s passed.' % test_expectation_id)
                        passed_test_cases += 1
                    else:
                        passed = False
                        print_to_log(test_fmt.format(expectation_id=test_expectation_id,
                                                     expected_content=expectation['content'],
                                                     original_content=original_content,
													reformatted_expected_content= expected_content,
													reformatted_content_found=content)
									)

                # Add file to list of files to remove
                files_to_remove.add(output_file)

        # Update number of test cases
        test_case_id += len(test_case['expectations'])

        # Remove all output files
        for my_file in files_to_remove:
            os.remove(my_file)

        # If all test cases passed, increment the number of passed benchmarks
        if passed:
            passed_benchmarks += 1
        else:
            print_to_screen(bcolors.ERROR + "FAILED!" + bcolors.NORMAL)

        # Increment the benchmark id
        benchmark_id += 1

    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # THE END
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    print_to_log('\n\n############################################################')

    # NOTE: ugly…
    total_benchmarks = benchmark_id - 1
    total_test_cases = test_case_id - 1

    if total_benchmarks == passed_benchmarks and total_test_cases == passed_test_cases:
        print_to_screen_and_log(
            'All benchmarks ({}/{}) passed successfully.'.format(passed_benchmarks, total_benchmarks))
        print_to_screen_and_log(
            'All test cases ({}/{}) passed successfully.'.format(passed_test_cases, total_test_cases))
    else:
        print_to_screen(bcolors.WARNING + 'WARNING! Some tests failed.' + bcolors.NORMAL)
        print_to_log('WARNING! Some tests failed.')

        if passed_benchmarks == total_benchmarks:
            print_to_screen('{2.GOOD}{0}/{1} benchmarks passed successfully.{2.NORMAL}'.format(passed_benchmarks,
                                                                                               total_benchmarks,
                                                                                               bcolors))
        else:
            print_to_screen('{2.WARNING}{0}/{1} benchmarks passed successfully.{2.NORMAL}'.format(passed_benchmarks,
                                                                                                  total_benchmarks,
                                                                                                  bcolors))

        print_to_log('{}/{} benchmarks passed successfully.'.format(passed_benchmarks, total_benchmarks))

        if passed_benchmarks < total_benchmarks:
            print_to_screen('{2.ERROR}{0}/{1} benchmarks failed.{2.NORMAL}'.format(
                total_benchmarks - passed_benchmarks,
                total_benchmarks, bcolors))
        else:
            print_to_screen('{}/{} benchmarks failed.'.format(total_benchmarks - passed_benchmarks, total_benchmarks))

        print_to_log('{}/{} benchmarks failed.'.format(total_benchmarks - passed_benchmarks, total_benchmarks))

        if passed_test_cases == total_test_cases:
            print_to_screen('{2.GOOD}{0}/{1} test cases passed successfully.{2.NORMAL}'.format(passed_test_cases,
                                                                                               total_test_cases,
                                                                                               bcolors))
        else:
            print_to_screen('{2.WARNING}{0}/{1} test cases passed successfully.{2.NORMAL}'.format(passed_test_cases,
                                                                                                  total_test_cases,
                                                                                                  bcolors))

        print_to_log('{}/{} test cases passed successfully.'.format(passed_test_cases, total_test_cases))

        if passed_test_cases < total_test_cases:
            print_to_screen('{2.ERROR}{0}/{1} test cases failed.{2.NORMAL}'.format(total_test_cases - passed_test_cases,
                                                                                   total_test_cases, bcolors))
        else:
            print_to_screen('{}/{} test cases failed.'.format(total_test_cases - passed_test_cases, total_test_cases))

        print_to_log('{}/{} test cases failed.'.format(total_test_cases - passed_test_cases, total_test_cases))

    print_to_screen('(See %s for details.)' % logfile_name)


# ************************************************************
# STARTING SCRIPT
# ************************************************************

# print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print_to_screen_and_log('############################################################')
print_to_screen('{0.BOLD} PolyOp TESTATOR{0.NORMAL}                                       v0.1'.format(bcolors))
print_to_log(' PolyOp TESTATOR                                       v0.1')
print_to_screen_and_log('')
print_to_screen_and_log(' Étienne André')
print_to_screen_and_log(' Université Paris 13, LIPN, CNRS, France')
print_to_screen_and_log('############################################################')
now = datetime.datetime.now()
print_to_screen_and_log(now.strftime("%A %d. %B %Y %H:%M:%S %z"))

# ************************************************************
# TESTING
# ************************************************************

# IMPORTING THE TESTS CONTENT
from regression_tests_data import tests

test(BINARY_NAME, tests, logfile, LOGFILE)

# ************************************************************
# THE END
# ************************************************************

print_to_screen_and_log('\n…The end of TESTATOR!')

sys.exit(0)
