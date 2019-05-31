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
			{'file': 'nothing.polyop.res' , 'content' : """
		BEGIN ANSWER
		I am very proud to do nothing.
		END ANSWER
			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/31
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Simple and tests',
		'input_files': ['simpletests.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'simpletests.polyop.res' , 'content' : """
(* and (p1 <= p2, p2 <= p1) *)
BEGIN ANSWER
p2 = p1
END ANSWER

(* and ((p1 <= p2), (p2 <= p1)) *)
BEGIN ANSWER
p2 = p1
END ANSWER
			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/31
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the time-elapsing',
		'input_files': ['elapsing.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'elapsing.polyop.res' , 'content' : """
(* elapsing (p1) in (p1 = 1 & p2 = 2) *)
BEGIN ANSWER
   p1 >= 1 & p2 = 2
END ANSWER

(* elapsing (p1) in (p1 = 5 & p2 >= 2) *)
BEGIN ANSWER
   p2 >= 2
 & p1 >= 5
END ANSWER

(* elapsing (p1) in (p1 = 5 & p2 >= p1) *)
BEGIN ANSWER
   p2 >= 5
 & p1 >= 5
END ANSWER

(* elapsing (p1, p3) in (p1 = 5 & p2 >= p1 &  p3 = 0) *)
BEGIN ANSWER
   p2 >= 5
 & p1 >= 5
 & p3 + 5 = p1
END ANSWER

(* elapsing (x1) in (p1 <= x1 & x1 <= p2 ) *)
BEGIN ANSWER
   p2 >= p1
 & x1 >= p1
END ANSWER


			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
### THE END
]
