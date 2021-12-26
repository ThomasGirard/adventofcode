module IntCode (
	VMState(..),
	ExecStatus(..),
	Adr(..),
	Val(..),
	initMem,
	initVM,
	execSync,
	execFully
	) where
	
{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
	
import qualified Data.Map.Strict as M
import Data.Function ((&), on)
import Debug.Trace (trace)
	
-- Avoid accidental conversions to/from different int types
newtype Val = Val Int
	deriving (Num, Ord, Eq, Show)
newtype Adr = Adr Int
	deriving (Num, Ord, Eq, Show, Enum)
newtype Ptr = Ptr Int
	deriving (Num, Ord, Eq, Show)
	
type Rel = Int
	
valAsPtr (Val v) = Ptr v
ptrAsAdr (Ptr v) = Adr v
valAsAdr (Val a) = Adr a
valToInt (Val a) = a

-- IntCode instructions
-- They contain Read arguments (RArg) and Write arguments (WArg) that have to be translated
-- at the time the instruction is executed.
data Instr = 
	  Halt 
	| BinOp Op RArg RArg WArg
	| Input WArg
	| Output RArg 
	| JmpIf JmpOp RArg RArg
	| SetRel RArg
	deriving (Ord, Eq, Show)
	
data WArg = 
	  WPosition Adr
	| WRelative Val
	deriving (Ord, Eq, Show)

data RArg = 
	  RPosition Adr
	| RImmediate Val
	| RRelative Val
	deriving (Ord, Eq, Show)

data JmpOp = JmpIfTrue | JmpIfFalse 
	deriving (Ord, Eq, Show)

data Op = 
	  Add
	| Mul
	| CmpLT
	| CmpEQ
	deriving (Ord, Eq, Show)
	
type Mem = M.Map Adr Val

-------------------------
-- INSTRUCTION DECODING
-------------------------	
	
-- Decode the instruction at the current pointer, and advance the pointer
decodeInstruction :: Mem -> Ptr -> (Instr, Ptr)
decodeInstruction mem p = case code of
	99 -> (Halt, p)
	1 -> decodeBinOp Add 
	2 -> decodeBinOp Mul
	7 -> decodeBinOp CmpLT
	8 -> decodeBinOp CmpEQ
	3 -> (Input (writeArg arg1), p+2)
	4 -> (Output (readArg arg1), p+2)
	5 -> (JmpIf JmpIfTrue  (readArg arg1) (readArg arg2), p+3)
	6 -> (JmpIf JmpIfFalse (readArg arg1) (readArg arg2), p+3)
	9 -> (SetRel (readArg arg1), p+2)
	n -> error $ "Unknown opcode " ++ (show n)
	where
	-- Read at offset n from the pointer
	readM n = mem M.! ((ptrAsAdr p) + n)
	-- Decode opcode and argument modes
	(code, arg1, arg2, arg3) = decodeCode $ readM 0
	
	decodeBinOp op = (BinOp op (readArg arg1) (readArg arg2) (writeArg arg3), p+4)
	
	readArg (ArgPos, n) = RPosition  $ valAsAdr $ readM n
	readArg (ArgRel, n) = RRelative  $ readM n
	readArg (ArgImm, n) = RImmediate $ readM n

	writeArg (ArgPos, n) = WPosition $ valAsAdr $ readM n
	writeArg (ArgRel, n) = WRelative $ readM n
	writeArg (ArgImm, _) = error "Immediate writes are illegal"

data ArgMode = ArgPos | ArgImm | ArgRel	
	deriving (Ord, Eq, Show)

-- See Day 5
decodeCode (Val n) = (op, (decMode c,1), (decMode b,2), (decMode a,3)) where
	op = n `mod` 100
	(a:b:c:_) = leftPad '0' 5 (show n)
	decMode '0' = ArgPos
	decMode '1' = ArgImm
	decMode '2' = ArgRel
	leftPad c n s | length s >= n = s
	leftPad c n s | length s < n = leftPad c n (c:s)
		
---------------
-- EXECUTION
---------------

-- Decode and execute a single instruction
exec1 :: VMState -> VMState
exec1 vm = case decodeInstruction (vMem vm) (vPtr vm) of
	(instr, p') -> let vm' = execInstr instr (vm {vPtr = p'}) in vm'
	
-- Execute a decoded instruction
execInstr :: Instr -> VMState -> VMState
execInstr instr vm = (go instr) {vStat = stat'} where
	mem = vMem vm
	rel = vRel vm
	readM adr = readOp adr mem rel
	writeM adr val = M.insert (writeOp adr mem rel) val mem
	stat' = getStat instr

	go Halt = vm

	go (Output a) = vm {vOut = (vOut vm ++ [o])} where
		o = valToInt $ readM a

	go (SetRel a) = vm {vRel = rel + relChange} where
		relChange = valToInt $ readM a

	go (JmpIf jm t j) = vm {vPtr = ptr'} where
		testVal = readM t
		jmpVal = readM j
		ptr' = if (jmpFun jm) testVal then valAsPtr jmpVal else vPtr vm

	go (Input w) = case vTape vm of
		(t:tape') -> vm {
			vMem = writeM w (Val t),
			vTape = tape'
			}
		[] -> error "Trying to read from empty tape"
		
	go (BinOp op a b w) = vm {
			vMem = writeM w newVal
			} where
				newVal = (binOpToFun op) (readM a) (readM b)
				
	-- go x = error $ "Unimplemented instruction " ++ (show x)
-- End execInstr				

binOpToFun op = case op of
	Add -> (+)
	Mul -> (*)
	CmpLT -> (boolToInt .) . (<)
	CmpEQ -> (boolToInt .) . (==)

getStat Halt = Halted
getStat (Output _) = HasOutput
getStat _ = Cont
					
jmpFun JmpIfTrue = (/=0)
jmpFun JmpIfFalse = (==0)

boolToInt True = 1
boolToInt False = 0
				
readOp :: RArg -> Mem -> Rel -> Val
readOp (RPosition adr) mem _ = mem M.! adr
readOp (RRelative (Val val)) mem rel = mem M.! (Adr $ val + rel)
readOp (RImmediate val) _ _ = val

writeOp :: WArg -> Mem -> Rel -> Adr
writeOp (WPosition adr) _ _ = adr
writeOp (WRelative (Val val)) _ rel = Adr $ val + rel

data VMState = VMState {
	vMem :: Mem,
	vTape :: Tape,
	vOut :: Output,
	vPtr :: Ptr,
	vRel :: Rel,
	vStat :: ExecStatus
} deriving (Ord, Eq, Show)

type Output = [Int]
type Tape = [Int]

data ExecStatus = 
	  Halted -- Program has ended
	| HasOutput -- Program is suspended after outputting a signal
	| Cont -- Any other state (the program can continue)
	deriving (Ord, Eq, Show)
	
execFully :: VMState -> VMState
execFully s = let s' = exec1 s in case vStat s' of
	Halted -> s'
	HasOutput -> execFully s'
	Cont -> execFully s'

-- For Day7, execute until either Halt, or Output
execSync :: VMState -> (VMState, Int)
execSync s = let s' = exec1 s in case vStat s' of
	Halted -> (s', last . vOut $ s')
	HasOutput -> (s', last . vOut $ s')
	Cont -> execSync s'
	
-- Initialize memory from a program listing
initMem = M.fromList . zip [Adr 0..] . map Val

-- Initialize a VM with input tape and memory
initVM :: Tape -> Mem -> VMState 
initVM tape mem = VMState {
	vMem = mem,
	vTape = tape,
	vOut = [],
	vPtr = Ptr 0,
	vRel = 0,
	vStat = Cont
}

