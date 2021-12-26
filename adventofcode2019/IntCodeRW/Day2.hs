{-# LANGUAGE ViewPatterns #-} 

import IntCode 

import qualified Data.Map.Strict as M
import Data.Function ((&), on)

part1 = execWith 12 2

part2 = 100 * noun' + verb' where
	(noun', verb') = head [(noun, verb) 
		| noun <- [0..99], verb <- [0..99]
		, execWith noun verb == Val 19690720]
	
execWith noun verb = 
	  initMem input 
	& M.insert (Adr 1) (Val noun)
	& M.insert (Adr 2) (Val verb)
	& initVM
	& execFully 
	& vMem
	& readOp (Position 0)
		
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0]

	