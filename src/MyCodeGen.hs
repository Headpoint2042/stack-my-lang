module MyCodeGen
    ( codeGen ) where

import Sprockell
import qualified MyParser
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

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
type ThreadCount = Int

type LocalLookup = Map VarName MemoryAddress
type GlobalLookup = Map VarName MemoryAddress


data Env = Env { nextLocalAddr  :: MemoryAddress    -- local addresses: 36
               , nextGlobalAddr :: MemoryAddress    -- shared addresses: 8
               , localLookup    :: LocalLookup      -- lookup map for local memory
               , globalLookup   :: GlobalLookup     -- lookup map for shared memory
               , freeRegs       :: [RegAddr]        -- list of available registers
               , mainCode       :: [Instruction]    -- code for the main thread
               , threadsCode    :: [[Instruction]]  -- code for new threads
               , threadCounter  :: ThreadCount
               } deriving (Show)

initialEnv :: Env
initialEnv = Env { nextLocalAddr  = 0
                 , nextGlobalAddr = 0
                 , localLookup    = Map.empty
                 , globalLookup   = Map.empty
                 , freeRegs       = initRegs
                 , mainCode       = []
                 , threadsCode    = []
                 , threadCounter  = 0
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

sampleAST :: MyParser.Program
sampleAST = MyParser.Program 
  [MyParser.Declaration 
    (MyParser.Primitive MyParser.Local MyParser.TInt "x" Nothing),
     MyParser.Print (MyParser.Var "x")]

add2nums :: MyParser.Program
add2nums = MyParser.Program [
         MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "x" (Just (MyParser.Const 1))),
         MyParser.Declaration (MyParser.Primitive MyParser.Global MyParser.TInt "y" (Just (MyParser.Const 2))),
         MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "z" Nothing),
         MyParser.Assignment (MyParser.Absolute "z" (MyParser.Add (MyParser.Var "x") (MyParser.Var "y"))),
         MyParser.Print (MyParser.Var "z")]

areatr :: MyParser.Program
areatr = MyParser.Program [
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "a" (Just (MyParser.Const 5))),
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "b" (Just (MyParser.Const 6))),
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "c" (Just (MyParser.Const 7))),
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "s" (Just (MyParser.Add (MyParser.Add (MyParser.Var "a") (MyParser.Var "b")) (MyParser.Var "c")))),
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "x" (Just (MyParser.Sub (MyParser.Sub (MyParser.Var "a") (MyParser.Var "b")) (MyParser.Var "c")))),
    MyParser.Declaration (MyParser.Primitive MyParser.Local MyParser.TInt "area" (Just (MyParser.Mult (MyParser.Mult (MyParser.Mult (MyParser.Var "s") (MyParser.Sub (MyParser.Var "s") (MyParser.Var "a"))) (MyParser.Sub (MyParser.Var "s") (MyParser.Var "b"))) (MyParser.Sub (MyParser.Var "s") (MyParser.Var "c"))))),
    MyParser.Print (MyParser.Var "s")
    , MyParser.Print (MyParser.Var "x")
    , MyParser.Print (MyParser.Var "area")
    ]




printMainCode :: Env -> IO ()
printMainCode env = putStrLn $ "Main Code: " ++ show (mainCode env)

-- TODO: REMOVE TESTING MAIN
main :: IO ()
main = do
  let env = compileProgram areatr
  printMainCode env
  run [mainCode env]



-- compile code for Program Block
compileProgram :: MyParser.Program -> Env
compileProgram (MyParser.Program programBlock) =
  let env = compileBlock initialEnv programBlock
  in env { mainCode = mainCode env ++ [EndProg] }


-- class for compilable data types of the EDSL
-- compilable is a class that modifies  the Env ? TODO rethink this comment
class Compilable a where
  -- return new Env that contains updated lookups, mainCode, and threadsCode
  compile :: Env -> a -> Env


-- compile code for Statement
instance Compilable MyParser.Statement where
  compile env stmt = case stmt of

    -- call compile on instances of Compilable
    MyParser.Declaration decl            -> compile env decl
    MyParser.Assignment  asgn            -> compile env asgn

    -- call special compile functions
    MyParser.Block       block           -> compileBlock env block
    MyParser.Print       expr            -> compilePrint  env expr
    MyParser.Lock        name            -> compileLock   env name
    MyParser.Unlock      name            -> compileUnlock env name

    MyParser.If cond thenBlock elseBlock -> compileIf     env cond thenBlock elseBlock
    MyParser.While cond whileBlock       -> compileWhile  env cond whileBlock
    MyParser.Thread threadBlock          -> compileThread env threadBlock


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
                         in if scope == MyParser.Global
                            then exprCode ++ [writeShMem reg addr]
                            else exprCode ++ [storeAddr reg addr]

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
        Just addr ->  let reg = getTmpReg env
                          exprCode = genExpr env expr reg
                          storeCode = exprCode ++ [storeAddr reg addr]
                          newMainCode = mainCode env ++ storeCode
                      in env { mainCode = newMainCode }

        -- variable not found in local lookup
        Nothing   ->
          -- search for variable in shared memory
          case Map.lookup name (globalLookup env) of

            -- found
            Just addr -> let reg = getTmpReg env
                             exprCode = genExpr env expr reg
                             storeCode = exprCode ++ [writeShMem reg addr]
                             newMainCode = mainCode env ++ storeCode
                          in env { mainCode = newMainCode }

            -- not found in shared memory
            Nothing   -> error $ "Variable " ++ name ++ " not found! Are you sure you declared it?"

    -- TODO: MyParser.Partial


-- compile code for Block
-- when leaving a block, we need to reset some values to their 
-- state before entering the block: nextLocalAddr, localLookup, freeRegs
-- remaining values are passed from within the block
-- instance Compilable MyParser.Block where
--   compile env (MyParser.Block stmts) =

--     -- save states of env before entering block
--     let oldLocalAddr    = nextLocalAddr env
--         oldLocalLookup  = localLookup   env
--         oldFreeRegs     = freeRegs      env

--         -- compile the list of statements
--         blockEnv = compileStatements env stmts

--         -- restore states of env after exiting block
--         newEnv = blockEnv { nextLocalAddr = oldLocalAddr
--                           , localLookup   = oldLocalLookup
--                           , freeRegs      = oldFreeRegs
--                           }
--     -- return new env
--     in newEnv


-- compile code for Block
-- when leaving a block, we need to reset some values to their 
-- state before entering the block: nextLocalAddr, localLookup, freeRegs
-- remaining values are passed from within the block
compileBlock :: Env -> MyParser.Block -> Env
compileBlock env stmts =

    -- save states of env before entering block
    let oldLocalAddr    = nextLocalAddr env
        oldLocalLookup  = localLookup   env
        oldFreeRegs     = freeRegs      env

        -- compile the list of statements
        blockEnv = compileStatements env stmts

        -- restore states of env after exiting block
        newEnv = blockEnv { nextLocalAddr = oldLocalAddr
                          , localLookup   = oldLocalLookup
                          , freeRegs      = oldFreeRegs
                          }
    -- return new env
    in newEnv


-- compile all statements and pass the new env between them
compileStatements :: Env -> [MyParser.Statement] -> Env
compileStatements = foldl compile
-- compileStatements env [] = env
-- compileStatements env (stmt:stmts) =
--   let newEnv = compile env stmt
--   in compileStatements newEnv stmts


-- compile code for Print
compilePrint :: Env -> MyParser.Expr -> Env
compilePrint env expr =
    let reg = getTmpReg env
        exprCode    = genExpr env expr reg
        printCode   = exprCode ++ [WriteInstr reg numberIO]
        newMainCode = mainCode env ++ printCode
    in env { mainCode = newMainCode }

-- get exprType
-- exprType :: Env -> MyParser.Expr -> MyParser.MyType
-- exprType env expr =

-- compile code for Lock
compileLock :: Env -> MyParser.VarName -> Env
compileLock env name =

    -- searcch for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- lock found
      Just addr  -> let lockCode    = getLock  env addr
                        newMainCode = mainCode env ++ lockCode
                    in env { mainCode = newMainCode }
      -- lock not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"


-- compile code for Unlock (similar to Lock)
compileUnlock :: Env -> MyParser.VarName -> Env
compileUnlock env name =

    -- search for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- found
      Just addr  -> let unlockCode  = releaseLock addr
                        newMainCode = mainCode env ++ unlockCode
                    in env { mainCode = newMainCode }
      -- not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"


-- compile code for If
compileIf :: Env -> MyParser.Condition -> MyParser.Block -> Maybe MyParser.Block -> Env
compileIf env cond thenBlock maybeElseBlock =

  -- generate not cond for conditional branch
  -- if reg false => do then, jump after else
  -- if reg true  => conditional branch to else
  let reg      = getTmpReg env
      condCode = genNotCond env cond reg

      -- save current state of mainCode
      oldMainCode = mainCode env

      -- compile thenBlock with current env
      thenEnv = compileBlock env thenBlock

      -- calculate length of then block (with old mainBlock)
      thenLength = length (mainCode thenEnv)

      -- handle maybe elseBlock
      (elseEnv, elseLength, jumpAfterElse) = case maybeElseBlock of

        -- else exists
        Just elseBlock -> let compiledElseEnv = compileBlock thenEnv elseBlock
                              elseLength = length (mainCode compiledElseEnv) - thenLength
                          in (compiledElseEnv, elseLength, elseLength + 1)

        -- else does not exist => jumpAfterElse is 1 (or we can use 0)
        Nothing -> (thenEnv, 0, 1)

      -- generate relative jumps
      -- thenLength + 1 (last instr of then) + 1 (branch after then) 
      jumpToElse = thenLength - length oldMainCode + 2

      -- generate if code
      -- depending on elseLength append fitting else instructions
      ifCode = condCode
               ++ [branchRel reg jumpToElse]
               ++ drop (length oldMainCode) (mainCode thenEnv)
               ++ [jumpRel jumpAfterElse]
               ++ (if elseLength > 0 then drop thenLength (mainCode elseEnv) else [])

      -- add ifCode to old mainCode
      newMainCode = oldMainCode ++ ifCode

  -- return elseEnv with updated mainCode
  in elseEnv { mainCode = newMainCode }


-- compile code for While
compileWhile :: Env -> MyParser.Condition -> MyParser.Block -> Env
compileWhile env cond whileBlock =

  -- structure: loop code ++ condition code

  -- generate cond for conditional branch
  -- if reg false => continue
  -- if reg true  => jump to start of loop
  let reg      = getTmpReg env
      condCode = genCond env cond reg

      -- save current state of mainCode
      oldMainCode = mainCode env

      -- generate code for while block with current env
      whileEnv = compileBlock env whileBlock

      -- calculate length of whileEnv (with old mainCode)
      whileLength = length (mainCode whileEnv) - length oldMainCode
      -- calculate length of condCode
      condLength  = length condCode

      -- calculate relative jumps
      -- jumpt to len (while - main) + 1 (last instr of while)
      jumpToCond  = whileLength + 1
      -- jumpToWhile = len cond + len while
      jumpToWhile = condLength + whileLength

      -- generate while code
      whileCode = [jumpRel jumpToCond]
                  ++ drop (length oldMainCode) (mainCode whileEnv)
                  ++ condCode
                  ++ [branchRel reg (-jumpToWhile)]

      -- update old mainCode
      newMainCode = oldMainCode ++ whileCode

  -- return whileEnv with new mainCode
  in whileEnv { mainCode = newMainCode }


-- compile code for Thread {Block}
compileThread :: Env -> MyParser.Block -> Env
compileThread env threadBlock =

  -- create unique signal variable and update counter
  let tCount = threadCounter env
      signalName = "threadSignal" ++ show tCount
      updatedEnv = env { threadCounter = tCount + 1 }

      -- add signal to shared mem
      (signalAddr, signalEnv) = addGlobalVariable signalName updatedEnv

      -- reset env for thread {block}
      threadEnv = initialEnv { nextGlobalAddr = nextGlobalAddr signalEnv
                             , globalLookup   = globalLookup   signalEnv
                             }

      -- compile thread {block}
      compiledThreadEnv = compileBlock threadEnv threadBlock

      -- create "jail" code for the thread
      -- getTmpReg from compiled threadEnv because each thread has separate regs
      -- signalReg can be 0 or 1 (because we will use testAndSet)
      -- this reg is temp because once the thread leaves his "jail" the reg becomes available again
      signalReg = getTmpReg compiledThreadEnv
      jailCode  = readShMem signalAddr signalReg  -- 2 instructions
                  ++ [flipReg signalReg]          -- 1 instruction
                  ++ [branchRel signalReg (-3)]

      -- add "jail" code to the mainCode of compiledThreadEnv
      tCode = jailCode ++ mainCode compiledThreadEnv ++ [EndProg]

      -- update threadsCode in env (or updatedEnv)
      newThreadsCode = tCode : threadsCode env

      -- create code to testAndSet the signal to 1 in shMem
      -- we will not read reg but we still need to receive from TestAndSet
      startSignalCode = let reg = getTmpReg updatedEnv
                        in testAndSet signalAddr reg

      -- update main code (we can use env or updatedEnv)
      newMainCode = mainCode env ++ startSignalCode

      -- update env with data from compiledThreadEnv
      newEnv = updatedEnv { nextGlobalAddr = nextGlobalAddr compiledThreadEnv
                          , globalLookup   = globalLookup compiledThreadEnv
                          , mainCode       = newMainCode
                          , threadsCode    = newThreadsCode
                          }
  -- return new env
  in newEnv


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
  -- occupy reg1 so that reg1 and reg2 are different
  -- use newEnv if you want to use list of freeRegs withou reg1
  -- use env if you want to use list of freeRegs with reg1
  let newEnv = occupyReg env reg1
      reg2 = getTmpReg newEnv
  in genExpr env e1 reg1
  ++ [Push reg1]
  ++ genExpr env e2 reg1
  ++ [Pop reg2]
  ++ [Compute op reg2 reg1 reg1]

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
genNotCond env cond reg =
     genCond env cond reg
  ++ [flipReg reg]

-- generate boolean
genBoolCond :: Env -> Bool -> RegAddr -> [Instruction]
genBoolCond env bool reg = [loadI (toInteger $ fromEnum bool) reg]


-------------------------------------------------------
--               SPROCKELL EXTENSIONS               --
-------------------------------------------------------

------------------------------
--     Manage Registers     --
------------------------------

initRegs :: [RegAddr]
initRegs = [regA, regB, regC, regD, regE, regF]

-- get a free register and remove it from the list of available registers
getReg :: Env -> (RegAddr, Env)
getReg env = case freeRegs env of
  []       -> error "No free registers!"
  (r:rs)   -> (r, env { freeRegs = rs })

-- TODO: remove this if obsolete
-- release a register -> adds it to the list of available registers
releaseReg :: RegAddr -> Env -> Env
releaseReg reg env = env { freeRegs = reg : freeRegs env }

-- occupy a register
occupyReg :: Env -> RegAddr -> Env
occupyReg env reg = 
  let newFreeRegs = filter (/= reg) (freeRegs env)
  in env { freeRegs = newFreeRegs }

-- sometimes we need to get and release a register in the same "block" of instructions
-- this allowes for easier use of temporary registers
getTmpReg :: Env -> RegAddr
getTmpReg env = case freeRegs env of
  []          -> error "No free registers!"
  (r:_)       -> r

-- flip the value of a reg (0 -> 1; 1 -> 0)
-- assume reg contains 0 or 1
flipReg :: RegAddr -> Instruction
flipReg reg = Compute Equal reg0 reg reg

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
getLock :: Env -> MemAddr -> [Instruction]
getLock env addr =
  let reg = getTmpReg env
  in testAndSet addr reg
  ++ [flipReg reg]
  ++ [branchRel reg (-3)]

-- releases a lock -> write 0 at shared MemAddr
releaseLock :: MemAddr -> [Instruction]
releaseLock addr = [writeShMem reg0 addr]

-- TestAndSet from 0 to 1 in shMem and receive response in reg: 1 - success; 0 - failure
testAndSet :: MemAddr -> RegAddr -> [Instruction]
testAndSet addr reg = TestAndSet (DirAddr addr) : [Receive reg]

---------------------------
--     Jump / Branch     --
---------------------------

-- jump relative
jumpRel :: CodeAddr -> Instruction
jumpRel addr = Jump (Rel addr)

-- branch relative if reg != 0
branchRel :: RegAddr -> CodeAddr -> Instruction
branchRel reg addr = Branch reg (Rel addr)