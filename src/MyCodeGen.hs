module MyCodeGen
    ( codeGen ) where

import Sprockell
import MyParser
import Data.Char

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

compileProgram :: Program -> [Instruction]
compileProgram (Program block) = compileBlock block

-- concatMap creates a list of of results after applying 
-- compileStatement to all elements in [Statements]
compileBlock :: Block -> [Instruction]
compileBlock = concatMap compileStatement


-- Statement
compileStatement :: Statement -> [Instruction]
compileStatement (Declaration decl) = compileDeclaration decl
compileStatement (Assignment assign) = compileAssignment assign
compileStatement (If cond thenBlock elseBlock) =
  let thenInstrs = compileBlock thenBlock
      elseInstrs = maybe [] compileBlock elseBlock
  in compileCondition cond ++
  [Branch (Rel (length thenInstrs + 1))]
  ++ thenInstrs ++
  [Jump (Rel (length elseInstrs + 1))]
  ++ elseInstrs

compileStatement (While cond body) =
  let bodyInstrs = compileBlock body
      condInstrs = compileCondition cond
  in condInstrs ++
  [Branch (Rel (length bodyInstrs + 2))]
  ++ bodyInstrs ++
  [Jump (Rel (-(length condInstrs + length bodyInstrs + 1)))]

compileStatement (Print expr) =
  compileExpr expr ++
  [WriteInstr (Reg 0) numberIO]  -- ??? how to print???
  
  -- Fork Join -> parbegin parend?
  -- Lock???

compileStatement (Block blk) = compileBlock blk

-- TODO


-- Declaration
compileDeclaration :: Declaration -> [Instruction]
compileDeclaration (Primitive scope prim) = compilePrimitive scope prim
compileDeclaration (Derived scope der) = compileDerived scope der

-- TODO: check if scope local or global; how to access shared memory?
-- make one general function for all primitives except lock???
-- Primitive
compilePrimitive :: Scope -> Primitive -> [Instruction]

-- Declare PInt
compilePrimitive scope (PInt name (Just expr)) = 
  compileExpr expr ++ [Store (Reg 0) (varNameToAddr name)]

compilePrimitive scope (PInt name Nothing) =
  [Store (ImmValue 0) (varNameToAddr name)]

-- Declare PBool
compilePrimitive scope (PBool name (Just expr)) =
  compileExpr expr ++ [Store (Reg 0) (varNameToAddr name)]

compilePrimitive scope (PBool name Nothing) =
  [Store (ImmValue 0) (varNameToAddr name)]



-- Derived
compileDerived :: Derived -> [Instruction]
compileDerived (Array typ name size (Just expr)) =
  compileExpr ++ 
  [Store (Reg 0) (varNameToAddr name)]
-- TODO cases for array nothing; string...


-- Assignment
compileAssignment :: Assignment -> [Instruction]
compileAssignment (Absolute name expr) =
  [Store (Reg 0) (varNameToAddr name)]

-- compileExpr index -> store address of target in reg 0?
compileAssignment (Partial name index value) =
  compileExpr index ++
  [Load (IndAddr (Reg 0)) (Reg 1)]
  ++ compileExpr value ++
  [Store (Reg 0) (IndAddr (Reg 1))]


-- for now only have compileExpr for const
-- how to keep track of nr of registers?
-- how to allocate addresses for variables?
-- load all new "things" in reg 0?
compileExpr :: Expr -> [Instruction]
compileExpr (Const n) = [Load (ImmValue n) (Reg 0)]
compileExpr (Var name) = [Load (varNameToAddr name) (Reg 0)]

-- use Stack for addition
-- need to somehow keep track of register number
compileExpr (Add e1 e2) =
  compileExpr e1 ++
  [ Push (Reg 0) ]
  ++ compileExpr e2 ++
  [ Pop (Reg 1)
  , Pop (Reg 0)
  , Compute Add (Reg 0) (Reg 1) (Reg 0)
  ]

compileExpr (Mult e1 e2) =
  compileExpr e1 ++
  [ Push (Reg 0) ]
  ++ compileExpr e2 ++
  [ Pop (Reg 1)
  , Pop (Reg 0)
  , Compute Mul (Reg 0) (Reg 1) (Reg 0)
  ]

-- is pop 1, pop 0 correct order?
compileExpr (Sub e1 e2) =
  compileExpr e1 ++
  [ Push (Reg 0) ]
  ++ compileExpr e2 ++
  [ Pop (Reg 1)
  , Pop (Reg 0)
  , Compute Sub (Reg 0) (Reg 1) (Reg 0)
  ]

  -- TODO: div

compileExpr (Condition cond) =
  compileCondition cond ++
  [] -- ???

-- assume reg 0 has address where to store c
compileExpr (Char c) =
  [ Load (ImmValue (ord c)) (Reg 0)]

-- same asumption TODO: not correct
compileExpr (StringLiteral s) =
  concatMap (\c -> [ Load (ImmValue (ord c)) (Reg 0) ]) s

compileExpr (ArrayLiteral exprs) = [] -- ????

compileExpr (ArrayIndex name idxExpr) =
  compileExpr idxExpr ++
  [ Load (varNameToAddr name) (Reg 1) -- ????
  ]


-- Condition
-- maybe have a special reg for conditions?
compileCondition :: Condition -> [Instruction]
compileCondition (Boolean b) =
  [ Load (ImmValue (if b then 1 else 0)) (Reg 0) ]

compileCondition (Eq c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  -- reg 0 has val of c1; reg 1 val of c2
  [ Pop (Reg 1) 
  , Compute Equal (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (Neq c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute NEq (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (Gt c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute Gt (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (Lt c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute Lt (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (GtE c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute GtE (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (LtE c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute LtE (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (And c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute And (Reg 0) (Reg 1) (Reg 0)
  ]

compileCondition (Or c1 c2) =
  compileCondition c1 ++
  [ Push (Reg 0) ]
  ++ compileCondition c2 ++
  [ Pop (Reg 1) 
  , Compute Or (Reg 0) (Reg 1) (Reg 0)
  ]

-- if c1 == 0 then = 1
-- if c1 == 1 then = 0
compileCondition (Not c1 c2) =
  compileCondition c1 ++
  [ Compute Equal (Reg 0) (ImmValue 0) (Reg 0) ]

compileCondition (Expr expr) = compileExpr expr

-- maybe create a table to keep track of all variables?
varNameToAddr :: String -> AddrImmDI
varNameToAddr name = ImmValue 0