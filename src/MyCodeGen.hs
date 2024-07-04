module MyCodeGen
    ( codeGen ) where

import Sprockell
import qualified MyParser
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import MyParser (block)

codeGen :: Integer -> [Instruction]
codeGen n = [
         Load (ImmValue $ fromInteger n) regE  -- upper bound is hardcoded
       , Load (ImmValue 0) regA                -- first number
       , Load (ImmValue 1) regB                -- second number

       -- "beginloop"
       , Compute Gt regA regE regC             -- regA > regE ?
       , Branch regC (Abs 12)                  -- then jump to target "end"
       , WriteInstr regA numberIO              -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC             -- regB > regE
       , Branch regC (Abs 12)                  -- target "end"
       , WriteInstr regB numberIO              -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))                       -- target "beginloop"

       -- "end"
       , EndProg
       ]


-- Lookup tables for local and global memory
type VarName = String
type MemoryAddress = Int

type LocalLookup = Map VarName MemoryAddress
type GlobalLookup = Map VarName MemoryAddress


data Env = Env { nextLocalAddr  :: MemoryAddress    -- local addresses: 36
               , nextGlobalAddr :: MemoryAddress    -- global addresses: 8
               , localLookup    :: LocalLookup
               , globalLookup   :: GlobalLookup
               , freeRegs       :: [RegAddr]        -- list of available registers
               , mainCode       :: [Instruction]    -- code for the main thread
               , threadsCode    :: [[Instruction]]  -- code for new threads
               } deriving (Show)

initialEnv :: Env
initialEnv = Env { nextLocalAddr  = 0
                 , nextGlobalAddr = 0
                 , localLookup    = Map.empty
                 , globalLookup   = Map.empty
                 , freeRegs       = [regA, regB, regC, regD, regE, regF]
                 , mainCode       = []
                 , threadsCode    = []
                 }


addLocalVariable :: VarName -> Env -> (MemoryAddress, Env)
addLocalVariable name env =
  let addr = nextLocalAddr env
      newLocalLookup = Map.insert name addr (localLookup env)
      newEnv = env { nextLocalAddr = addr + 1, localLookup = newLocalLookup }
  in (addr, newEnv)


addGlobalVariable :: VarName -> Env -> (MemoryAddress, Env)
addGlobalVariable name env =
  let addr = nextGlobalAddr env
      newGlobalLookup = Map.insert name addr (globalLookup env)
      newEnv = env { nextGlobalAddr = addr + 1, globalLookup = newGlobalLookup }
  in (addr, newEnv)


--------------------------------------------------
--               CODE GENERATION                --
--------------------------------------------------

-- class for compilable data types of the EDSL
-- compilable is a class that modifies  the Env ? TODO rethink this comment
class Compilable a where
  -- return new Env that contains updated lookups, mainCode, and threadsCode
  compile :: Env -> a -> Env

instance Compilable MyParser.Statement where
  compile env stmt = case stmt of

    -- call compile
    MyParser.Declaration decl            -> compile env decl
    MyParser.Assignment  asgn            -> compile env asgn
    MyParser.Print       _               -> compile env stmt
    MyParser.Lock        _               -> compile env stmt
    MyParser.Unlock      _               -> compile env stmt

    -- TODO
    -- MyParser.If cond thenBlock elseBlock -> compile env cond thenBlock elseBlock
    -- MyParser.While cond whileBlock       -> compile env cond whileBlock

    
    -- MyParser.Thread threadBlock          -> compile env threadBlock
    -- MyParser.Block  block                -> compile env block


-- compile code for Declaration
instance Compilable MyParser.Declaration where
  compile env decl = case decl of

    -- Primitive
    MyParser.Primitive scope typ name maybeExpr ->
      -- get addr and newEnv
      let (addr, newEnv) = if scope == MyParser.Global
                           then addGlobalVariable name env
                           else addLocalVariable  name env

          -- reg will be used to store the value of the variable (expr or default)
          reg = getTmpReg env
          declCode = case maybeExpr of
          
            -- evaluate expr and store in memory at addr
            Just expr -> let exprCode = genExpr env expr reg
                         in exprCode ++ [storeAddr reg addr]

            -- store default value depending on type at memory addr
            Nothing   -> case typ of
              -- default: 0
              MyParser.TInt   -> let defaultCode = [loadI 0 reg]
                                 in if scope == MyParser.Global
                                    then defaultCode ++ [writeShMem reg addr]
                                    else defaultCode ++ [storeAddr reg addr]

              -- default: 0 = False
              MyParser.TBool  -> let defaultCode = [loadI 0 reg]
                                 in if scope == MyParser.Global
                                    then defaultCode ++ [writeShMem reg addr]
                                    else defaultCode ++ [storeAddr reg addr]

              -- default: 32 = ' '; instead of 32 we can use (fromInteger $ ord ' ')
              MyParser.TChar  -> let defaultCode = [loadI 32 reg]
                                 in if scope == MyParser.Global
                                    then defaultCode ++ [writeShMem reg addr]
                                    else defaultCode ++ [storeAddr reg addr]
            
          -- update main code
          newMainCode = mainCode newEnv ++ declCode

      in newEnv { mainCode = newMainCode }

    -- TLock
    MyParser.TLock name ->
      -- locks are stored in shared memory
      let (addr, newEnv) = addGlobalVariable name env
          reg = getTmpReg env
          lockCode = [loadI 0 reg, storeAddr reg addr]
          newMainCode = mainCode newEnv ++ lockCode
      in newEnv { mainCode = newMainCode }

    --TODO: Array and String


-- compile code for Assignment
instance Compilable MyParser.Assignment where
  compile env asgn = case asgn of

    -- Absolute
    MyParser.Absolute name expr ->

      -- search for variable in local memory
      case Map.lookup name (localLookup env) of

        -- variable found in local lookup
        Just addr -> let reg = getTmpReg
                         exprCode = genExpr env expr reg
                         storeCode = exprCode ++ [storeAddr reg addr]
                         newMainCode = mainCode env ++ storeCode
                      in env { mainCode = newMainCode }

        -- variable not found in local lookup
        Nothing   ->

          -- search for variable in shared memory
          case Map.lookup name (globalLookup env) of

            -- found
            Just addr -> let reg = getTmpReg
                             exprCode = genExpr env expr reg
                             storeCode = exprCode ++ [writeShMem reg addr]
                             newMainCode = mainCode env ++ storeCode
                          in env { mainCode = newMainCode }

            -- not found in shared memory
            Nothing   -> error $ "Variable " ++ name ++ " not found! Are you sure you declared it?"
    
    -- TODO: MyParser.Partial


-- compile code for Print
instance Compilable MyParser.Print where
  compile env (MyParser.Print expr) =
    let reg = getTmpReg
        exprCode  = genExpr env expr reg
        printCode = exprCode ++ [WriteInstr reg numberIO]
        newMainCode = mainCode env ++ printCode
    in env { mainCode = newMainCode }


-- compile code for Lock
instance Compilable MyParser.Lock where
  compile env (MyParser.Lock name) =

    -- searcch for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- lock found
      Just addr  -> let lockCode = getLock addr 
                        newMainCode = mainCode env ++ lockCode
                    in env { mainCode = newMainCode }
      -- lock not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"
                   

-- compile code for Unlock
instance Compilable MyParser.Unlock where
  compile env (MyParser.Unlock name) =

    -- search for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- found
      Just addr  -> let unlockCode = releaseLock addr
                        newMainCode = mainCode env ++ unlockCode
                    in env { mainCode = newMainCode }
      -- not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"

      




-- generate Expr
-- evaluates expr and stores result in reg
genExpr :: Env -> MyParser.Expr -> RegAddr -> [Instruction]
genExpr env expr reg = case expr of

  -- constant, char, variable, condition
  MyParser.Const     val  -> [loadI val reg]
  MyParser.Char      val  -> [loadI (toInteger $ ord val) reg]
  MyParser.Var       name -> loadVar env name reg
  MyParser.Condition cond -> genCond env cond reg

  -- binary operations
  MyParser.Add  e1 e2     -> genBinExpr env Add e1 e2 reg
  MyParser.Mult e1 e2     -> genBinExpr env Mul e1 e2 reg
  MyParser.Sub  e1 e2     -> genBinExpr env Sub e1 e2 reg
  -- MyParser.Div  e1 e2 -> genDiv env e1 e2 reg

  -- derived types TODO

-- generate Condition
-- evaluates cond and stores result in reg
genCond :: Env -> MyParser.Condition -> RegAddr -> [Instruction]
genCond env cond reg = case cond of

  -- binary operations
  MyParser.Eq  c1 c2 -> genBinCond env Equal c1 c2 reg
  MyParser.Neq c1 c2 -> genBinCond env NEq   c1 c2 reg
  MyParser.Gt  c1 c2 -> genBinCond env Gt    c1 c2 reg
  MyParser.Lt  c1 c2 -> genBinCond env Lt    c1 c2 reg
  MyParser.Ge  c1 c2 -> genBinCond env GtE   c1 c2 reg
  MyParser.Le  c1 c2 -> genBinCond env LtE   c1 c2 reg
  MyParser.And c1 c2 -> genBinCond env And   c1 c2 reg
  MyParser.Or  c1 c2 -> genBinCond env Or    c1 c2 reg

  -- unary operation (Not), Boolean, Expr
  MyParser.Not     c -> genNotCond  env c reg
  MyParser.Boolean b -> genBoolCond env b reg
  MyParser.Expr    e -> genExpr     env e reg

-- generate code for Expr binary operations (except division):
-- Add, Mult, Sub
genBinExpr :: Env -> Operator -> MyParser.Expr -> MyParser.Expr -> RegAddr -> [Instruction]
genBinExpr env op e1 e2 reg1 =
  let reg2 = getTmpReg env
  in genExpr env e1 reg1
  ++ [Push reg1]
  ++ genExpr env e2 reg1
  ++ [Pop reg2]
  ++ [Compute op reg1 reg2 reg1]

-- generate code for integer division
-- genDiv :: Env -> MyParser.Expr -> MyParser.Expr -> RegAddr -> [Instruction]
-- genDiv env e1 e2 reg1 =
--   let reg2  = getTmpReg env

-- generate code for Condition binary operations:
-- Eq, Neq, Gt, Lt, Ge, Le, And, Or
genBinCond :: Env -> Operator -> MyParser.Condition -> MyParser.Condition -> RegAddr -> [Instruction]
genBinCond env op c1 c2 reg1 =
  let reg2 = getTmpReg env
  in genCond env c1 reg1
  ++ [Push reg1]
  ++ genCond env c2 reg1
  ++ [Pop reg2]
  ++ [Compute op reg1 reg2 reg1]

-- generate negation of a condition
-- condition can be 1 or 0 => check if reg0 == cond:
-- 0 == 0 => 1;  0 == 1 => 0
genNotCond :: Env -> MyParser.Condition -> RegAddr -> [Instruction]
genNotCond env cond reg1 =
     genCond env cond reg1
  ++ [Compute Equal reg0 reg1 reg1]

-- generate boolean
genBoolCond :: Env -> Bool -> RegAddr -> [Instruction]
genBoolCond env bool reg = [loadI (toInteger $ fromEnum bool) reg]


-------------------------------------------------------
--               SPROCKELL EXTENSIONS               --
-------------------------------------------------------

------------------------------
--     Manage Registers     --
------------------------------

-- get a free register and remove it from the list of available registers
getReg :: Env -> (RegAddr, Env)
getReg env = case freeRegs env of
  []       -> error "No free registers!"
  (r:rs)   -> (r, env { freeRegs = rs })

-- TODO: remove this if obsolete
-- release a register -> adds it to the list of available registers
releaseReg :: RegAddr -> Env -> Env
releaseReg reg env = env { freeRegs = reg : freeRegs env }

-- sometimes we need to get and release a register in the same "block" of instructions
-- this allowes for easier use of temporary registers
getTmpReg :: Env -> RegAddr
getTmpReg env = case freeRegs env of
  []          -> error "No free registers!"
  (r:_)       -> r


-------------------------------
--     Load in Registers     --
-------------------------------

type Offset = Integer

-- load an immediate value to a register
loadI :: Integer -> RegAddr -> Instruction
loadI val = Load (ImmValue $ fromInteger val)

-- load value from address contained in reg1 to reg2 
load :: RegAddr -> RegAddr -> Instruction
load reg1 = Load (IndAddr reg1)

-- load value from MemAddr into reg 
loadAddr :: MemAddr -> RegAddr -> Instruction
loadAddr addr = Load (DirAddr addr)

-- load after an immediate value to reg3
-- loadAI env addr offset target
loadAI :: Env -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI env reg1 offset reg3 =
  let reg2 = getTmpReg env
  in [loadI offset reg2]
  ++ [Compute Add reg1 reg2 reg2]
  ++ [Load (IndAddr reg2) reg3]

-- load a primitive variable from memory to reg
loadVar :: Env -> VarName -> RegAddr -> [Instruction]
loadVar env name reg = case Map.lookup name (localLookup env) of
  -- load from local memory
  Just addr -> [loadAddr addr reg]
  Nothing   -> case Map.lookup name (globalLookup env) of
    -- load from shMem
    Just addr -> readShMem addr reg
    Nothing   -> error $ "Variable " ++ name ++ " not found! Are you sure you defined it?"

-- copy a value from reg1 to reg2
copyReg :: RegAddr -> RegAddr -> Instruction
copyReg = Compute Add reg0

-- stores value from reg to MemAddr
storeAddr :: RegAddr -> MemAddr -> Instruction
storeAddr reg addr = Store reg (DirAddr addr)

-- stores value from reg to addr plus offset
storeAI :: Env -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI env reg1 reg2 offset =
  let reg3 = getTmpReg env
  in [loadI offset reg3]
  ++ [Compute Add reg2 reg3 reg3]
  ++ [Store reg1 (IndAddr reg3)]

---------------------------
--     Shared Memory     --
---------------------------

-- request to read shMem addr and receive result in reg
readShMem :: MemAddr -> RegAddr -> [Instruction]
readShMem addr reg = [ReadInstr (DirAddr addr), Receive reg]

-- write content of reg to shMem addr
writeShMem :: RegAddr -> MemAddr -> Instruction
writeShMem reg addr = WriteInstr reg (DirAddr addr)

-- try to acquire the lock for MemAddr
-- reg is used to check if lock is free or not
getLock :: MemAddr -> [Instruction]
getLock addr =
  let reg = getTmpReg env
  in [TestAndSet (DirAddr addr)]
  ++ [Receive reg]
  ++ [Compute Equal reg0 reg reg]
  ++ [Branch reg (Rel $ -3)]

-- releases a lock -> write 0 at shared MemAddr
releaseLock :: MemAddr -> [Instruction]
releaseLock addr = [writeShMem reg0 addr]



-- compileProgram :: MyParser.Program -> [Instruction]
-- compileProgram (MyParser.Program block) = compileBlock block

-- -- concatMap creates a list of of results after applying
-- -- compileStatement to all elements in [Statements]
-- compileBlock :: MyParser.Block -> [Instruction]
-- compileBlock = concatMap compileStatement


-- Statement
-- compileStatement :: MyParser.Statement -> [Instruction]
-- compileStatement (MyParser.Declaration decl) = compileDeclaration decl
-- compileStatement (MyParser.Assignment assign) = compileAssignment assign
-- compileStatement (MyParser.If cond thenBlock elseBlock) =
--   let thenInstrs = compileBlock thenBlock
--       elseInstrs = maybe [] compileBlock elseBlock
--   in compileCondition cond ++
--   [Branch (Rel (length thenInstrs + 1))]
--   ++ thenInstrs ++
--   [Jump (Rel (length elseInstrs + 1))]
--   ++ elseInstrs

-- compileStatement (MyParser.While cond body) =
--   let bodyInstrs = compileBlock body
--       condInstrs = compileCondition cond
--   in condInstrs ++
--   [Branch (Rel (length bodyInstrs + 2))]
--   ++ bodyInstrs ++
--   [Jump (Rel (-(length condInstrs + length bodyInstrs + 1)))]

-- compileStatement (MyParser.Print expr) =
--   compileExpr expr ++
--   [WriteInstr (Reg 0) numberIO]  -- ??? how to print???

--   -- Fork Join -> parbegin parend?
--   -- Lock???

-- compileStatement (MyParser.Block blk) = compileBlock blk

-- -- TODO


-- -- Declaration
-- compileDeclaration :: MyParser.Declaration -> [Instruction]
-- compileDeclaration (MyParser.Primitive scope prim) = compilePrimitive scope prim
-- -- compileDeclaration (Derived scope der) = compileDerived scope der

-- -- TODO: check if scope local or global; how to access shared memory?
-- -- make one general function for all primitives except lock???
-- -- Primitive
-- compilePrimitive :: MyParser.Scope -> MyParser.Primitive -> [Instruction]

-- -- Declare PInt
-- compilePrimitive scope (MyParser.PInt name (Just expr)) = 
--   compileExpr expr ++ [Store (Reg 0) (varNameToAddr name)]

-- compilePrimitive scope (MyParser.PInt name Nothing) =
--   [Store (ImmValue 0) (varNameToAddr name)]

-- -- Declare PBool
-- compilePrimitive scope (MyParser.PBool name (Just expr)) =
--   compileExpr expr ++ [Store (Reg 0) (varNameToAddr name)]

-- compilePrimitive scope (MyParser.PBool name Nothing) =
--   [Store (ImmValue 0) (varNameToAddr name)]



-- -- Derived
-- compileDerived :: MyParser.Derived -> [Instruction]
-- compileDerived (MyParser.Array typ name size (Just expr)) =
--   compileExpr ++ 
--   [Store (Reg 0) (varNameToAddr name)]
-- -- TODO cases for array nothing; string...


-- -- Assignment
-- compileAssignment :: MyParser.Assignment -> [Instruction]
-- compileAssignment (MyParser.Absolute name expr) =
--   [Store (Reg 0) (varNameToAddr name)]

-- -- compileExpr index -> store address of target in reg 0?
-- compileAssignment (MyParser.Partial name index value) =
--   compileExpr index ++
--   [Load (IndAddr (Reg 0)) (Reg 1)]
--   ++ compileExpr value ++
--   [Store (Reg 0) (IndAddr (Reg 1))]


-- -- for now only have compileExpr for const
-- -- how to keep track of nr of registers?
-- -- how to allocate addresses for variables?
-- -- load all new "things" in reg 0?
-- compileExpr :: MyParser.Expr -> [Instruction]
-- compileExpr (MyParser.Const n) = [Load (ImmValue n) (Reg 0)]
-- compileExpr (MyParser.Var name) = [Load (varNameToAddr name) (Reg 0)]

-- -- use Stack for addition
-- -- need to somehow keep track of register number
-- compileExpr (MyParser.Add e1 e2) =
--   compileExpr e1 ++
--   [ Push (Reg 0) ]
--   ++ compileExpr e2 ++
--   [ Pop (Reg 1)
--   , Pop (Reg 0)
--   , Compute Add (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileExpr (MyParser.Mult e1 e2) =
--   compileExpr e1 ++
--   [ Push (Reg 0) ]
--   ++ compileExpr e2 ++
--   [ Pop (Reg 1)
--   , Pop (Reg 0)
--   , Compute Mul (Reg 0) (Reg 1) (Reg 0)
--   ]

-- -- is pop 1, pop 0 correct order?
-- compileExpr (MyParser.Sub e1 e2) =
--   compileExpr e1 ++
--   [ Push (Reg 0) ]
--   ++ compileExpr e2 ++
--   [ Pop (Reg 1)
--   , Pop (Reg 0)
--   , Compute Sub (Reg 0) (Reg 1) (Reg 0)
--   ]

--   -- TODO: div

-- compileExpr (MyParser.Condition cond) =
--   compileCondition cond ++
--   [] -- ???

-- -- assume reg 0 has address where to store c
-- compileExpr (MyParser.Char c) =
--   [ Load (ImmValue (ord c)) (Reg 0)]

-- -- same asumption TODO: not correct
-- compileExpr (MyParser.StringLiteral s) =
--   concatMap (\c -> [ Load (ImmValue (ord c)) (Reg 0) ]) s

-- compileExpr (MyParser.ArrayLiteral exprs) = [] -- ????

-- compileExpr (MyParser.ArrayIndex name idxExpr) =
--   compileExpr idxExpr ++
--   [ Load (varNameToAddr name) (Reg 1) -- ????
--   ]


-- -- Condition
-- -- maybe have a special reg for conditions?
-- compileCondition :: MyParser.Condition -> [Instruction]
-- compileCondition (MyParser.Boolean b) =
--   [ Load (ImmValue (if b then 1 else 0)) (Reg 0) ]

-- compileCondition (MyParser.Eq c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   -- reg 0 has val of c1; reg 1 val of c2
--   [ Pop (Reg 1) 
--   , Compute Equal (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Neq c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute NEq (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Gt c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute Gt (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Lt c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute Lt (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Ge c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute GtE (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Le c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute LtE (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.And c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute And (Reg 0) (Reg 1) (Reg 0)
--   ]

-- compileCondition (MyParser.Or c1 c2) =
--   compileCondition c1 ++
--   [ Push (Reg 0) ]
--   ++ compileCondition c2 ++
--   [ Pop (Reg 1) 
--   , Compute Or (Reg 0) (Reg 1) (Reg 0)
--   ]

-- -- if c1 == 0 then = 1
-- -- if c1 == 1 then = 0
-- compileCondition (MyParser.Not c1) =
--   compileCondition c1 ++
--   [ Compute Equal (Reg 0) (ImmValue 0) (Reg 0) ]

-- compileCondition (MyParser.Expr expr) = compileExpr expr

-- maybe create a table to keep track of all variables?
-- varNameToAddr :: String -> Sprockell.AddrImmDI
-- varNameToAddr name = ImmValue 0
