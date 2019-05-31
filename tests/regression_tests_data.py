#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       PolyOp
#
#             Data for non-regression tests
#
# Université Paris 13, LIPN, CNRS, France
#
# File description: non-regression tests data
#
# File contributors : Étienne André
#
# Created           : 2019/05/31
# Last modified     : 2019/05/31
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests = [
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/31
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the nothing operation',
		'input_files': ['nothing.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nothing.res' , 'content' : """
		nothing
			"""
			} #end statespace file
		] # end expectations
	} # end test case
	,
	#------------------------------------------------------------
### THE END
]
