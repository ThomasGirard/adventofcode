module IntCode (
	Val(..),
	Adr(..),
	Ptr(..),
	execFully,
	initMem,
	readAdr
	) where
	
import qualified Data.Map.Strict as M
import Data.Function ((&), on)
	
newtype Val = Val Int
	deriving (Num, Ord, Eq, Show)
newtype Adr = Adr Int
	deriving (Num, Ord, Eq, Show, Enum)
newtype Ptr = Ptr Int
	deriving (Num, Ord, Eq, Show)
	
ptrAsAdr (Ptr v) = Adr v
valAsAdr (Val a) = Adr a

data Instr = 
	  Halt 
	| BinOp Op Adr Adr Adr
	| Input Adr
	| Output Adr 
	deriving (Ord, Eq, Show)

data Op = 
	  Add
	| Mul
	deriving (Ord, Eq, Show)
	
type Mem = M.Map Adr Val

type ExecStatus = Maybe () -- Nothing on continue, Just () on exit

initMem = M.fromList . zip [Adr 0..]

readInstr :: Mem -> Ptr -> (Instr, Ptr)
readInstr mem p = case mem M.! q of
	99 -> (Halt, p)
	1 -> readBinOp Add 
	2 -> readBinOp Mul
	3 -> (Input (param 1), p+2)
	4 -> (Output (param 1), p+2)
	where
	param n = valAsAdr $ mem M.! (q+n)
	q = ptrAsAdr p
	readBinOp op = (BinOp op (param 1) (param 2) (param 3), p+4)
	

step :: (Mem, Ptr) -> (ExecStatus, (Mem, Ptr))
step (mem, p) = case readInstr mem p of
	(instr, p') -> let (es, mem') = execInstr instr mem in (es, (mem', p'))
	
execInstr :: Instr -> Mem -> (ExecStatus, Mem)
execInstr Halt mem = (Just (), mem)
execInstr instr mem = (Nothing, mem') where
	mem' = case instr of
		BinOp op a b w -> M.insert w (f (readAdr a mem) (readAdr b mem)) mem where
			f = case op of
				Add -> (+)
				Mul -> (*)

readAdr adr mem = mem M.! adr
	
execFully :: (Mem, Ptr) -> Mem
execFully s = let (es, s') = step s in case es of
	Just () -> fst s'
	Nothing -> execFully s'
	