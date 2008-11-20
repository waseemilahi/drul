#! /usr/bin/env python
"""
DruL team, Columbia (2008) PLT class
copyright DruL team

contact: tb2332@columbia.edu

name: LaunchTests.py
language: python
programer: Thierry Bertin-Mahieux

main program of the test suite, launch all tests that it can find.
"""


import os
import sys
import glob
import time
import tempfile



drulpath = "../"
testspath = "./Tests/"
logspath = "./LOGS/"
mainprog = "../Parser/interpret"

# returns a list of file in current dir
# to use with os.walk
def grab_tests(arg=list(),path="",names=""):
    tests = glob.glob(os.path.join(os.path.abspath(path),'*.drultest'))
    for t in tests:
        arg.append(t)
    return arg


# make sure that all tests found have a corresponding output
# if not, program exits
def make_sure_tests_have_outputs(tests):
    noout = list()
    for t in tests :
        if not os.path.exists(t + 'out'):
            print (t+'out')
            noout.append(t)
        if len(noout) > 0:
            print 'problem,',len(noout),'tests have no corresponding output'
            print 'we stop testing......... go solve it! and grab a beer'
            print 'files that cause problems:'
            for t in noout:
                print t
            sys.exit(0)


# launch any command, return outputs
def command_with_output(cmd):
    if not type(cmd) == unicode :
        cmd = unicode(cmd,'utf-8')
    #should this be a part of slashify or command_with_output?
    #if sys.platform=='darwin' :
    #    cmd = unicodedata.normalize('NFC',cmd)

    (child_stdin,child_stdout,child_stderr) = os.popen3(cmd.encode('utf-8'))
    data1 = child_stdout.read()
    data2 = child_stderr.read()
    child_stdout.close()
    child_stderr.close()
    return (data1,data2)


# launch one test, given a test path, returns output lines
# (output is first written to a file, than read)
def launch_one_test(tpath):
    #cmd = 'head -20 ' + tpath
    cmd = mainprog + " < '" + tpath + "'"
    (outdata,outerr) = command_with_output(cmd)
    # write to a tempfile, then read it
    # dumb, but easier to compare with a saved output file
    tempfname = "tempfileTODELETE.txt"
    tempf = open(tempfname,'w')
    tempf.write(outdata)
    tempf.write(outerr)
    tempf.close()
    outlines = read_file(tempfname)
    os.unlink(tempfname)
    return outlines



# read file given a path, return lines
def read_file(p):
    fIn = open(p,'r')
    res = fIn.readlines()
    fIn.close()
    return res


# compare two list of lines, returns true or false
def compare_2set_of_lines(lines1,lines2):
    if len(lines1) != len(lines2):
        return False
    for k in range(len(lines1)):
        if lines1[k] != lines2[k]:
            return False
    return True


# create_log_file, returns a path
# if path already exists, add something at the end
def create_log_file():
    res = "LOG_tests_"
    res += str(time.ctime()).replace(' ','_')
    res += '.log'
    res = os.path.abspath(os.path.join(logspath,res))
    if os.path.exists(res):
        counter = 1
        while os.path.exists(res):
            counter = counter + 1
            res = res[:-4] + '(' + str(counter) + ').log'
    return res


# add lines to a log path, can pass in one string or list of string
def add_to_log(logf,lines):
    # open log file, creates it if needed
    #if os.path.exists(logf):
    #    flog = open(logf,'w')
    #else:
    #    flog = open(logf,'a')
    flog = open(logf,'a')
    # if string
    if type(lines) == type(" "):
        flog.write(lines + '\n')
    else:
        for l in lines:
            flog.write(l + '\n')
    # close
    flog.close()



# help menu
def die_with_usage():
    print '*********************************************************'
    print 'Welcome to DruL test suite'
    print 'to launch test, type:'
    print '   LaunchTests.py -go'
    print ''
    print 'test files should end in: .drultest'
    print 'and corresponding outputs: .drultestout'
    print 'Of course, test names must match, like:'
    print "'testpattern1.drultest' and 'testpattern1.drultestout'"
    print '*********************************************************'
    sys.exit(0)


#**************************************************************
# MAIN

if __name__ == '__main__' :

    # launch help menu if needed
    if len(sys.argv) < 2 or sys.argv[1] != "-go":
        die_with_usage()


    # check if testing program exists and can be found
    if not os.path.exists(mainprog):
        print "you didn't install the main program, make interpret"
        sys.exit(0)


    # grab all tests
    tests = list()
    os.path.walk(testspath,grab_tests,tests)

    # make sure all tests have an output
    make_sure_tests_have_outputs(tests)

    # make sure we found tests
    if len(tests) == 0:
        print "dummass, there's no tests"
        sys.exit(0)
    else :
        print 'launching',len(tests),'tests'

    # get logfile
    logfile = create_log_file()


    # launch every test
    counter = 0
    countpassed = 0
    countfailed = 0
    for t in tests:
        counter = counter + 1
        newout = launch_one_test(t)
        goodout = read_file(t + 'out')
        isOK = compare_2set_of_lines(newout,goodout)
        if isOK:
            countpassed = countpassed + 1
            add_to_log(logfile,str(counter) + ') test PASSED: '+t)
        else :
            countfailed += 1
            add_to_log(logfile,str(counter) + ') test FAILED: '+t)
            add_to_log(logfile,'******************************************')
            add_to_log(logfile,'*should be:*')
            add_to_log(logfile,goodout)
            add_to_log(logfile,'*and it is:*')
            add_to_log(logfile,newout)
            add_to_log(logfile,'******************************************')

    # results
    print 'passed',countpassed,'tests out of',counter
    add_to_log(logfile,'########## SUMMARY:')
    add_to_log(logfile,'passed '+str(countpassed)+' tests out of '+str(counter))
