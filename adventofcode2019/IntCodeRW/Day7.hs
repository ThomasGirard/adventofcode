{-# LANGUAGE ViewPatterns #-} 

import IntCode 

import qualified Data.Map.Strict as M
import Data.Function ((&), on)
import Data.List (permutations)
import Debug.Trace (trace)

part1 = maximum . map runPermutation . permutations $ [0..4] where
	runPermutation perm = 
		perm
		& map initAmp
		& foldl runAmp 0
		
	runAmp extraTape amp = 
		amp
		& appendTape extraTape
		& execFully
		& head . vOut
	
initAmp n = 
	initMem input
	& initVM
	& appendTape n
		
appendTape extraTape vm = vm {vTape = vTape vm ++ [extraTape]}

part2 = maximum . map runPermutation2 . permutations $ [5..9]
runPermutation2 perm = 
	perm
	& map initAmp
	& runAmps 0

-- Pass the signal through then one of two things will happen
-- Either the last amp has halted and we stop
-- Or we loop back
runAmps :: Int -> [VMState] -> Int
runAmps out0 [a,b,c,d,e] = let
	(a', outA) = execSync (appendTape out0 a)
	(b', outB) = execSync (appendTape outA b)
	(c', outC) = execSync (appendTape outB c)
	(d', outD) = execSync (appendTape outC d)
	(e', outE) = execSync (appendTape outD e)
	in case vStat e' of 
		Halted -> outE 
		otherwise -> runAmps outE [a',b',c',d',e'] 
-- TODO this is not very elegant, there has to be a way to make this a neat fold
		
input = [3,8,1001,8,10,8,105,1,0,0,21,42,63,76,101,114,195,276,357,438,99999,3,9,101,2,9,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,101,4,9,9,102,5,9,9,1001,9,5,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,101,5,9,9,102,3,9,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99]
