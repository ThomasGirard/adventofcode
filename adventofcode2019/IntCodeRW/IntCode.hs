module IntCode (
	Val(..),
	Adr(..),
	Ptr(..),
	ReadOp(..),
	execFully,
	initMem,
	readOp,
	VMState,
	initVM,
	vMem,
	vTape,
	vOut,
	ExecStatus(..),
	decodeCode
	) where
	
import Control.Monad.State
	
import qualified Data.Map.Strict as M
import Data.Function ((&), on)
import Debug.Trace (trace)
	
newtype Val = Val Int
	deriving (Num, Ord, Eq, Show)
newtype Adr = Adr Int
	deriving (Num, Ord, Eq, Show, Enum)
newtype Ptr = Ptr Int
	deriving (Num, Ord, Eq, Show)
	
ptrAsAdr (Ptr v) = Adr v
valAsAdr (Val a) = Adr a
valToInt (Val a) = a

data ReadOp = 
	  Position Adr
	| Immediate Val
	deriving (Ord, Eq, Show)

data Instr = 
	  Halt 
	| BinOp Op ReadOp ReadOp Adr
	| Input Adr
	| Output ReadOp 
	deriving (Ord, Eq, Show)

data Op = 
	  Add
	| Mul
	deriving (Ord, Eq, Show)
	
type Mem = M.Map Adr Val

data ExecStatus = 
	  Cont 
	| Halted 
	| HasOutput Int
	deriving (Ord, Eq, Show)
	
decodeInstruction :: Mem -> Ptr -> (Instr, Ptr)
decodeInstruction mem p = case code of
	99 -> (Halt, p)
	1 -> decodeBinOp Add 
	2 -> decodeBinOp Mul
	3 -> (Input (param 1), p+2)
	4 -> (Output (decRead mode1 1), p+2)
	where
	(code, mode1, mode2, mode3) = decodeCode $ mem M.! q
	param n = valAsAdr $ mem M.! (q+n)
	q = ptrAsAdr p
	decodeBinOp op = (BinOp op (decRead mode1 1) (decRead mode2 2) (param 3), p+4)
	decRead DImm n = Immediate $ mem M.! (q+n)
	decRead DPos n = Position $ valAsAdr $ mem M.! (q+n)
	
data DecReadMode = DPos | DImm 	
	deriving (Ord, Eq, Show)

decodeCode (Val n) = (op, decMode c, decMode b, decMode a) where
	op = n `mod` 100
	(a:b:c:_) = leftPad '0' 5 (show n)
	
decMode '0' = DPos
decMode '1' = DImm
	
leftPad c n s | length s >= n = s
leftPad c n s | length s < n = leftPad c n (c:s)
	
---------------
-- Execution
---------------
	
exec1 :: VMState -> (ExecStatus, VMState)
exec1 vm = case decodeInstruction (vMem vm) (vPtr vm) of
	(instr, p') -> let (es, vm') = execInstr instr vm in (es, vm' {vPtr = p'})
	
execInstr :: Instr -> VMState -> (ExecStatus, VMState)
execInstr Halt vm = (Halted, vm)
execInstr (Output a) vm = (HasOutput o, vm') where
	o = valToInt $ readOp a (vMem vm)
	vm' = vm {vOut = (vOut vm ++ [o])}
execInstr instr vm = (Cont, vm') where
	mem = vMem vm
	vm' = case instr of
		BinOp op a b w -> 
			vm {vMem = M.insert w (f (readOp a mem) (readOp b mem)) mem} where
			f = case op of
				Add -> (+)
				Mul -> (*)
		Input w -> case vTape vm of
			[] -> error "Tried to read from empty tape"
			(t:tape') -> vm {
				vMem = M.insert w (Val t) mem,
				vTape = tape'
				}

readOp :: ReadOp -> Mem -> Val
readOp (Position adr) mem = mem M.! adr
readOp (Immediate val) _ = val

type Output = [Int]
type Tape = [Int]

type VM = State VMState

data VMState = VMState {
	vMem :: Mem,
	vTape :: Tape,
	vOut :: Output,
	vPtr :: Ptr
} deriving (Ord, Eq, Show)

execFully :: VMState -> VMState
execFully s = let (es, s') = exec1 s in case es of
	Halted -> s'
	HasOutput o -> execFully s'
	Cont -> execFully s'
	
	
initMem = M.fromList . zip [Adr 0..] . map Val
	
initVM :: Mem -> VMState 
initVM mem = VMState {
	vMem = mem,
	vTape = [],
	vOut = [],
	vPtr = Ptr 0
}
