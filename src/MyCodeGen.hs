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


-- Lookup table


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
varNameToAddr :: String -> Sprockell.AddrImmDI
varNameToAddr name = ImmValue 0
