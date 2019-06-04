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
# Last modified     : 2019/06/04
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests = [
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/31
		# Test last modified       : 2019/05/31
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
		# Test since               : 2019/06/04
		# Test last modified       : 2019/06/04
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test very basic syntax',
		'input_files': ['syntax.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'syntax.polyop.res' , 'content' : """
BEGIN ANSWER
  a = 0
& b = 1
END ANSWER


(*--------------------*)

(* OPERATION 2: 
simplify(  a = 0
& b = 1)
*)
BEGIN ANSWER
  a = 0
& b = 1
END ANSWER


(*--------------------*)

(* OPERATION 3: 
simplify(  a = 0
or
   b = 1)
*)
BEGIN ANSWER
  a = 0
or
   b = 1
END ANSWER


(*--------------------*)

(* OPERATION 4: 
simplify(  a = 0
or
   b = 1)
*)
BEGIN ANSWER
  a = 0
or
   b = 1
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
		# Test last modified       : 2019/06/03
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Simple conjunction tests',
		'input_files': ['simpletests.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'simpletests.polyop.res' , 'content' : """
(* and (p1 <= p2, p2 <= p1) *)
BEGIN ANSWER
p1 = p2
END ANSWER

(* and ((p1 <= p2), (p2 <= p1)) *)
BEGIN ANSWER
p1 = p2
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
		# Test last modified       : 2019/05/31
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
	{
		# Test version             : 1
		# Test since               : 2019/06/03
		# Test last modified       : 2019/06/03
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the point exhibition operation (1 dimension)',
		'input_files': ['point-1dim.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'point-1dim.polyop.res' , 'content' : """
(* OPERATION 1: 
exhibit(  p1 = 2)
*)
BEGIN ANSWER
p1=2
END ANSWER


(*--------------------*)

(* OPERATION 2: 
exhibit(  p1 >= 1
& 2 >= p1)
*)
BEGIN ANSWER
p1=1
END ANSWER


(*--------------------*)

(* OPERATION 3: 
exhibit(  p1 > 1
& 2 >= p1)
*)
BEGIN ANSWER
p1=3/2
END ANSWER


(*--------------------*)

(* OPERATION 4: 
exhibit(  p1 > 1
& 2 > p1)
*)
BEGIN ANSWER
p1=3/2
END ANSWER


(*--------------------*)

(* OPERATION 5: 
exhibit(  p1 > 2)
*)
BEGIN ANSWER
p1=3
END ANSWER


(*--------------------*)

(* OPERATION 6: 
exhibit(  p1 >= 2)
*)
BEGIN ANSWER
p1=2
END ANSWER


(*--------------------*)

(* OPERATION 7: 
exhibit(  5 >= p1)
*)
BEGIN ANSWER
p1=1
END ANSWER


(*--------------------*)

(* OPERATION 8: 
exhibit(  5 > p1)
*)
BEGIN ANSWER
p1=1
END ANSWER


(*--------------------*)

(* OPERATION 9: 
exhibit(  1 > p1)
*)
BEGIN ANSWER
p1=0
END ANSWER


(*--------------------*)

(* OPERATION 10: 
exhibit(  0 >= p1)
*)
BEGIN ANSWER
p1=-1
END ANSWER


(*--------------------*)

(* OPERATION 11: 
exhibit(  0 > p1)
*)
BEGIN ANSWER
p1=-1
END ANSWER


(*--------------------*)

(* OPERATION 12: 
exhibit(  0 > 2045 + p1)
*)
BEGIN ANSWER
p1=-2046
END ANSWER


(*--------------------*)

(* OPERATION 13: 
exhibit(false)
*)
BEGIN ANSWER
ERROR! Empty constraint
END ANSWER


(*--------------------*)

(* OPERATION 14: 
exhibit(  p1 = 1)
*)
BEGIN ANSWER
p1=1
END ANSWER


(*--------------------*)

(* OPERATION 15: 
exhibit(false)
*)
BEGIN ANSWER
ERROR! Empty constraint
END ANSWER


(*--------------------*)

(* OPERATION 16: 
exhibit(false)
*)
BEGIN ANSWER
ERROR! Empty constraint
END ANSWER


(*--------------------*)

(* OPERATION 17: 
exhibit(true)
*)
BEGIN ANSWER
p1=1
END ANSWER

			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/03
		# Test last modified       : 2019/06/03
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the point exhibition operation (2 dimensions)',
		'input_files': ['point-2dim.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'point-2dim.polyop.res' , 'content' : """
(* OPERATION 1: 
exhibit(  p1 = 2)
*)
BEGIN ANSWER
p1=2, p2=1
END ANSWER


(*--------------------*)

(* OPERATION 2: 
exhibit(  p1 > 2)
*)
BEGIN ANSWER
p1=3, p2=1
END ANSWER


(*--------------------*)

(* OPERATION 3: 
exhibit(  0 > p1)
*)
BEGIN ANSWER
p1=-1, p2=1
END ANSWER


(*--------------------*)

(* OPERATION 4: 
exhibit(  p2 > 3
& p1 > p2)
*)
BEGIN ANSWER
p2=4, p1=5
END ANSWER


(*--------------------*)

(* OPERATION 5: 
exhibit(  5 > p1
& p1 > p2)
*)
BEGIN ANSWER
p2=1, p1=3
END ANSWER


(*--------------------*)

(* OPERATION 6: 
exhibit(  p1 > 3
& p1 = p2)
*)
BEGIN ANSWER
p1=4, p2=4
END ANSWER


(*--------------------*)

(* OPERATION 7: 
exhibit(  p1 > 5
& p1 + 2 = p2)
*)
BEGIN ANSWER
p1=6, p2=8
END ANSWER


(*--------------------*)

(* OPERATION 8: 
exhibit(  p1 + 1 > p2
& p1 > 5
& p2 > p1)
*)
BEGIN ANSWER
p1=6, p2=13/2
END ANSWER


			"""
			} #end statespace file
		] # end expectations
	} # end test case
			

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/03
		# Test last modified       : 2019/06/03
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the point exhibition operation (3 dimensions)',
		'input_files': ['point-3dim.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'point-3dim.polyop.res' , 'content' : """
(* OPERATION 1: 
exhibit(  p2 > p3
& p1 > p2)
*)
BEGIN ANSWER
p2=1, p1=2, p3=0
END ANSWER


(*--------------------*)

(* OPERATION 2: 
exhibit(  p1 > p2
& p2 = p3)
*)
BEGIN ANSWER
p2=1, p1=2, p3=1
END ANSWER


(*--------------------*)

(* OPERATION 3: 
exhibit(  p1 > p2
& p3 > p2)
*)
BEGIN ANSWER
p2=1, p1=2, p3=2
END ANSWER


(*--------------------*)

(* OPERATION 4: 
exhibit(  p2 + p3 > p1
& p1 > p2)
*)
BEGIN ANSWER
p2=1, p1=2, p3=2
END ANSWER


(*--------------------*)

(* OPERATION 5: 
exhibit(  p2 > p3
& p3 > 5
& p1 > p2)
*)
BEGIN ANSWER
p2=6, p1=7, p3=11/2
END ANSWER


(*--------------------*)

(* OPERATION 6: 
exhibit(  p2 > p3
& 2 > p1
& p1 > p2)
*)
BEGIN ANSWER
p2=1, p1=3/2, p3=0
END ANSWER


(*--------------------*)

(* OPERATION 7: 
exhibit(  p2 > 0
& p2 > p3
& p1 > p2)
*)
BEGIN ANSWER
p2=1, p1=2, p3=0
END ANSWER


(*--------------------*)

(* OPERATION 8: 
exhibit(  0 > p3
& p1 > 0
& p2 = 0)
*)
BEGIN ANSWER
p2=0, p1=1, p3=-1
END ANSWER



			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	#------------------------------------------------------------
	
	,
	
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/04
		# Test last modified       : 2019/06/04
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the zonepred operation',
		'input_files': ['zonepred.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'zonepred.polyop.res' , 'content' : """
(* OPERATION 1: 
zonepred (  x >= 0
& x = y,   x >= 2
& x = 2 + y,   x = 2
& y = 0, x, y, y)
*)
BEGIN ANSWER
  x = 2
& y = 2
END ANSWER


(*--------------------*)

(* OPERATION 2: 
zonepred (  x >= 0
& x = y,   x >= 2
& x = 2 + y,   x = 2
& y = 0, x, y, y)
*)
BEGIN ANSWER
  x = 2
& y = 2
END ANSWER


(*--------------------*)

(* OPERATION 3: 
zonepred (  x >= 0
& x = y,   x >= 2
& x = 2 + y,   x = 4
& y = 0, x, y, y)
*)
BEGIN ANSWER
  x = 4
& y = 4
END ANSWER


(*--------------------*)

(* OPERATION 4: 
zonepred (  x >= 0
& x = y,   x >= 2
& x = 2 + y,   x = 4
& y = 1, x, y, y)
*)
BEGIN ANSWER
  4 >= x
& x >= 3
& x = y
END ANSWER


(*--------------------*)

(* OPERATION 5: 
zonepred (  p >= 0
& x >= 0
& x = y,   p >= 0
& x >= p
& p + y = x,   p >= 0
& x = 5
& y = 0, x, y, y)
*)
BEGIN ANSWER
  p >= 0
& x = 5
& y = 5
END ANSWER


(*--------------------*)

(* OPERATION 6: 
zonepred (  x >= 0
& p >= 0
& 8 >= x
& x = y,   x >= p
& p >= 0
& 8 >= p
& x = p + y,   p >= 0
& x = 5
& y = 0, x, y, y)
*)
BEGIN ANSWER
  p >= 0
& x = 5
& y = 5
END ANSWER


(*--------------------*)

(* OPERATION 7: 
zonepred (  x >= 0
& p >= 0
& 8 >= x
& x = y,   x >= p
& p >= 0
& 8 >= p
& x = p + y,   x = 10
& p = 6
& y = 4, x, y, y)
*)
BEGIN ANSWER
  8 >= x
& x >= 6
& p = 6
& x = y
END ANSWER

			"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	### THE END
]
