#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       PolyOp
#
#             Data for non-regression tests
#
# Université Sorbonne Paris Nord, LIPN, CNRS, France
#
# File description: non-regression tests data
#
# File contributors : Étienne André
#
# Created           : 2019/05/31
# Last modified     : 2024/03/12
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
			} #end result file
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
			} #end result file
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
			} #end result file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2023/07/11
		# Test last modified       : 2023/07/11
		# Test for PolyOp version  : 1.3
		'purpose'    : 'Simple disjunction tests',
		'input_files': ['union.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'union.polyop.res' , 'content' : """
 (* OPERATION 1:
 union(  a = 0 ,   a > 0)
 *)
 BEGIN ANSWER
   a >= 0
 END ANSWER

 (* OPERATION 2:
 union(  a = 0 ,   a > 0 ,   0 > a)
 *)
 BEGIN ANSWER
 true
 END ANSWER

 (* OPERATION 3:
 union(  a = 0 ,   b = 1)
 *)
 BEGIN ANSWER
   a = 0
 or
    b = 1
 END ANSWER

 (* OPERATION 4:
 union(  a = 0
 & b = 0 ,   a > 0 ,   b > 0)
 *)
 BEGIN ANSWER
   a = 0
 & b = 0
 or
    a > 0
 or
    b > 0
 END ANSWER

 (* OPERATION 5:
 union(  a = b ,   a > b ,   b > 0)
 *)
 BEGIN ANSWER
   a >= b
 or
    b > 0
 END ANSWER

			"""
			} #end result file
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
			} #end result file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2024/03/12
		# Test last modified       : 2024/03/12
		# Test for PolyOp version  : 1.0
		'purpose'    : 'Test the diff',
		'input_files': ['diff.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'diff.polyop.res' , 'content' : """
(* OPERATION 1:
diff(  p >= 3
& 10 >= p ,   p > 4
& 5 >= p)
*)
BEGIN ANSWER
  p > 5
& 10 >= p
or
   p >= 3
& 4 >= p
END ANSWER


(*--------------------*)

(* OPERATION 2:
diff(  p >= 3
& 10 >= p ,   p > 4
& 5 >= p)
*)
BEGIN ANSWER
  p > 5
& 10 >= p
or
   p >= 3
& 4 >= p
END ANSWER


(*--------------------*)

(* OPERATION 3:
diff(  p2 >= 0
& p1 > p2 ,   3 > p2
& p1 = 10)
*)
BEGIN ANSWER
  p1 > 10
& p2 >= 0
& p1 > p2
or
   10 > p1
& p2 >= 0
& p1 > p2
or
   10 > p2
& p2 >= 3
& p1 = 10
END ANSWER
			"""
			} #end result file
		] # end expectations
	} # end test case

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2024/01/09
		# Test last modified       : 2024/01/09
		# Test for PolyOp version  : 1.4
		'purpose'    : 'Test the hide and project operations',
		'input_files': ['hide-project.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'hide-project.polyop.res' , 'content' : """
 (* OPERATION 1:
 hide (p1) in (true)
 *)
 BEGIN ANSWER
 true
 END ANSWER

 (* OPERATION 2:
 hide (p1) in (false)
 *)
 BEGIN ANSWER
 false
 END ANSWER

 (* OPERATION 3:
 hide (p1) in (false)
 *)
 BEGIN ANSWER
 false
 END ANSWER

 (* OPERATION 4:
 hide (p1) in (  p1 = 0)
 *)
 BEGIN ANSWER
 true
 END ANSWER

 (* OPERATION 5:
 hide (p1) in (  p1 = 0
 & p2 = 5)
 *)
 BEGIN ANSWER
   p2 = 5
 END ANSWER

 (* OPERATION 6:
 project (  p2 = 5
 & p1 = 0) onto (p2)
 *)
 BEGIN ANSWER
   p2 = 5
 END ANSWER

 (* OPERATION 7:
 hide (p2) in (  p2 >= p1
 & p3 >= p2)
 *)
 BEGIN ANSWER
   p3 >= p1
 END ANSWER

 (* OPERATION 8:
 project (  p3 >= p2
 & p2 >= p1) onto (p1, p3)
 *)
 BEGIN ANSWER
   p3 >= p1
 END ANSWER

 (* OPERATION 9:
 project (  p3 >= p2
 & p2 >= p1) onto (p1, p3)
 *)
 BEGIN ANSWER
   p3 >= p1
 END ANSWER
			"""
			} #end result file
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
			} #end result file
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
			} #end result file
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
			} #end result file
		] # end expectations
	} # end test case
			
	#------------------------------------------------------------
	
	,
	
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/04
		# Test last modified       : 2019/06/04
		# Test for PolyOp version  : 1.1
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
			} #end result file
		] # end expectations
	} # end test case
			
	,
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/18
		# Test last modified       : 2019/06/18
		# Test for PolyOp version  : 1.2
		'purpose'    : 'Test the update operation',
		'input_files': ['updates.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'updates.polyop.res' , 'content' : """
(* OPERATION 1: 
update () in (true)
*)
BEGIN ANSWER
true
END ANSWER


(*--------------------*)

(* OPERATION 2: 
update () in (true)
*)
BEGIN ANSWER
true
END ANSWER


(*--------------------*)

(* OPERATION 3: 
update () in (  x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 4: 
update (x := x + 0) in (  x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 5: 
update (x := x + 0) in (  x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 6: 
update (x := 1) in (  x = 0)
*)
BEGIN ANSWER
  x = 1
END ANSWER


(*--------------------*)

(* OPERATION 7: 
update (x := 1) in (  x > 0)
*)
BEGIN ANSWER
  x = 1
END ANSWER


(*--------------------*)

(* OPERATION 8: 
update (x := 1, y := 2) in (  y = 0
& x = 0)
*)
BEGIN ANSWER
  y = 2
& x = 1
END ANSWER


(*--------------------*)

(* OPERATION 9: 
update (x := 1, y := 2) in (true)
*)
BEGIN ANSWER
  y = 2
& x = 1
END ANSWER


(*--------------------*)

(* OPERATION 10: 
update (x := 1) in (  2 > y
& x = 2)
*)
BEGIN ANSWER
  2 > y
& x = 1
END ANSWER


(*--------------------*)

(* OPERATION 11: 
update (y := 2046) in (  y > x
& 2047 > z
& z > y)
*)
BEGIN ANSWER
  2047 > z
& z > x
& y = 2046
END ANSWER


(*--------------------*)

(* OPERATION 12: 
update ((y := x + 0, x := y + 0)) in (  y = 2
& x = 1)
*)
BEGIN ANSWER
  y = 1
& x = 2
END ANSWER

(*--------------------*)

(* OPERATION 13: 
update ((y := 2*y + x + 5, x := y + 0)) in (  y = 2
& x = 1)
*)
BEGIN ANSWER
y = 10
& x = 2
END ANSWER


			"""
			} #end result file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/06/18
		# Test last modified       : 2019/06/18
		# Test for PolyOp version  : 1.2
		'purpose'    : 'Test the zonepredgu operation',
		'input_files': ['zonepredgr.polyop'],
		'options'    : '',
		'expectations' : [
			{'file': 'zonepredgr.polyop.res' , 'content' : """
(* OPERATION 1: 
zonepredgu (  x = 0, true, (),   x = 0, , true, (),   x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 2: 
zonepredgu (  x = 0, true, (x := 0),   x = 0, , true, (x := 0),   x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 3: 
zonepredgu (  x = 0, true, (x := x + 0),   x = 0, , true, (x := x + 0),   x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 4: 
zonepredgu (  x >= 0,   x = 3, (x := 1),   x >= 1, x, true, (),   x = 2)
*)
BEGIN ANSWER
  x = 1
END ANSWER


(*--------------------*)

(* OPERATION 5: 
zonepredgu (  x >= 0
& x = y,   x = 3, (x := 1),   x >= 1
& x + 2 = y, x, y, true, (),   x = 2
& y = 4)
*)
BEGIN ANSWER
  x = 1
& y = 3
END ANSWER


(*--------------------*)

(* OPERATION 6: 
zonepredgu (  x >= 0,   x = 3, (),   x >= 1, , true, (),   x = 2)
*)
BEGIN ANSWER
false
END ANSWER


(*--------------------*)

(* OPERATION 7: 
zonepredgu (  x = 0,   x = 0, (x := 0),   x >= 0
& 1 >= x, x,   x = 1, (x := 0),   x = 0)
*)
BEGIN ANSWER
  x = 0
END ANSWER


(*--------------------*)

(* OPERATION 8: 
zonepredgu (  x >= 0
& x = y,   x = 3, (x := 1),   x >= 1
& x + 2 = y, x, y,   y = 5, (y := 0),   x = 3
& y = 0)
*)
BEGIN ANSWER
  x = 1
& y = 3
END ANSWER


(*--------------------*)

(* OPERATION 9: 
zonepredgu (  x >= 0
& y >= 0,   x = 3, (x := 1),   x >= 1
& x + 2 = y, x, y,   y = 5, (y := 0),   x = 3
& y = 0)
*)
BEGIN ANSWER
  x = 1
& y = 3
END ANSWER
			"""
			} #end result file
		] # end expectations
	} # end test case
			
	,
			### THE END
]
