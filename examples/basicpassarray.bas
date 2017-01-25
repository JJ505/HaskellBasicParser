--Jalen Johnson
import Text.ParserCombinators.Parsec
-- hiding (spaces)
import System.Environment
import Control.Monad
import System.Random
import Control.Monad.Trans
import Control.Monad.State
import Data.Array.IO
--a big help was using
--https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing


{-
"Case Sensitive" = False
"Start Symbol"   = <Lines>
{String Chars} = {Printable} - ["]
{WS}           = {Whitespace} - {CR} - {LF}
NewLine        = {CR}{LF}|{CR}
Whitespace     = {WS}+
ID             = {letter}
String         = '"'{String Chars}*'"'
Integer        = {digit}+
<Lines>       ::= Integer <Statements> '\n' <Lines>
                | Integer <Statements> '\n'
<Statements>  ::= <Statement> ':' <Statements>
                | <Statement>
<Statement>   ::= DIM <Array List>
                | END
                | FOR ID '=' <Expression> TO <Expression>
                | FOR ID '=' <Expression> TO <Expression> STEP <Expression>
                | GOTO Integer
                | GOSUB Integer
                | IF <Expression> THEN Integer
                | INPUT String ';' <ID List>
                | LET <Variable> '=' <Expression>
                | NEXT <ID List>
                | ON <Expression> GOTO <Integer List>
                | PRINT <Print list>
                | PRINT TAB '(' <Expression> ')' <Print list>
                | REM {Printable}*
                | RETURN
                | <Variable> '=' <Expression>
<ID List>  ::= ID ',' <ID List>
             | ID
<Array List>      ::= <Array> ',' <Array List>
                    | <Array>
<Integer List>    ::= Integer ',' <Integer List>
                    | Integer
<Expression List> ::= <Expression> ',' <Expression List>
                    | <Expression>
<Print List>      ::= <Expression> ',' <Print List>
                    | <Expression> ';' <Print List>
                    | <Expression>
                    |
<Expression>  ::= <And Exp> OR <Expression>
                | <And Exp>
<And Exp>     ::= <Not Exp> AND <And Exp>
                | <Not Exp>
<Not Exp>     ::= NOT <Compare Exp>
                | <Compare Exp>
<Compare Exp> ::= <Add Exp> '='  <Compare Exp>
                | <Add Exp> '<>' <Compare Exp>
                | <Add Exp> '>'  <Compare Exp>
                | <Add Exp> '>=' <Compare Exp>
                | <Add Exp> '<'  <Compare Exp>
                | <Add Exp> '<=' <Compare Exp>
                | <Add Exp>
<Add Exp>     ::= <Mult Exp> '+' <Add Exp>
                | <Mult Exp> '-' <Add Exp>
                | <Mult Exp>
<Mult Exp>    ::= <Negate Exp> '*' <Mult Exp>
                | <Negate Exp> '/' <Mult Exp>
                | <Negate Exp>
<Negate Exp>  ::= '-' <Power Exp>
                | <Power Exp>
<Power Exp>   ::= <Value> '^' <Power Exp>
                | <Value>
<Value>       ::= '(' <Expression> ')'
                | <Variable>
                | <Function>
                | <Constant>
<Variable>    ::= ID
                | <Array>
<Array>       ::= ID '(' <Expression List> ')'
<Function>    ::= INT '(' <Expression> ')'
                | RND '(' <Expression> ')'
<Constant> ::= Integer
             | String
           -}
data Expr = Op String (Expr -> Expr -> Expr) Expr Expr | Negative Expr | Grouped Expr | Function String Expr | Id {char' :: Char} | Integer' {integer' :: Int} | Float' {float' :: Float}| String' String | Error String | Semicolon | Comma | Newline | Trueval | Falseval | Dimensions {name :: Expr, size :: Int}

instance Show Expr where
    show (Op cs _ e e') = show e ++ cs ++ show e'
    show (Negative e) = "-" ++ show e
    show (Grouped e) = "(" ++ show e ++ ")"
    show (Id c) = [c]
    show (Integer' n) = show n
    show (Float' n) = show n
    show (String' cs) = cs
    show (Function cs e) = cs ++ show e
    show (Error cs) = cs
    show (Newline) = "\n"
    show (Semicolon) = " "
    show (Comma) = "\t"


data Stmt = REM String | FOR {index :: Expr, start :: Expr , stop :: Expr, step :: Expr} | IF {test :: Expr, conseq :: [Stmt], alt :: [Stmt]} | PRINT [Expr] | GOTO {line :: Int} | GOSUB {line :: Int} | INPUT {prompt :: String, vars :: [Expr]} | LET {lhs :: Expr, rhs :: Expr} | NEXT {indices' :: [Expr]} | RETURN | END
--REM stands for remark

instance Show Stmt where
    show (REM cs) = "REM " ++ cs
    show (FOR i e e' (Integer' 1)) = "FOR " ++ show i ++ "=" ++ show e ++ " TO " ++ show e'
    show (FOR i e e' e'') = "FOR " ++ show i ++ "=" ++ show e ++ " TO " ++ show e' ++ " STEP " ++ show e''
    show (IF r [GOTO n] []) = "IF " ++ show r ++ " THEN " ++ show n
    show (IF r es []) = "IF " ++ show r ++ " THEN " ++ showWith ':' es
    show (IF r es es') = "IF " ++ show r ++ " THEN " ++ showWith ':' es ++ " ELSE " ++ showWith ':' es'
    show (PRINT []) = "PRINT"
    show (PRINT es) = "PRINT " ++ concatMap show es
    show (GOTO n) = "GOTO " ++ show n
    show (GOSUB n) = "GOSUB " ++ show n
    show (INPUT "" is) = "INPUT " ++ showWith ',' is
    show (INPUT cs is) = "INPUT " ++ show cs ++ ";" ++ showWith ',' is
    show (LET i e) = "LET " ++ show i ++ "=" ++ show e
    show (NEXT is) = "NEXT " ++ showWith ',' is
    show RETURN = "RETURN"
    show END = "END"


showWith _ [] = ""
showWith _ [x] = show x
showWith c (x:xs) = show x ++ [c] ++ showWith c xs

showLine (n, [stmt]) = putStrLn $ show n ++ " " ++ show stmt
showLine (n, stmts) = putStrLn $ show n ++ " " ++ showWith ':' stmts

list program = mapM_ showLine program


equals :: Expr -> Expr -> Expr
equals (Integer' a) (Integer' b) = if a == b then Trueval else Falseval
equals (Negative (Integer' a)) (Negative (Integer' b)) = if a == b then Trueval else Falseval
equals (Negative (Integer' a)) (Integer' b) = if a == 0 && b == 0 then Trueval else Falseval
equals (Integer' a) (Negative (Integer' b)) = if a == 0 && b == 0 then Trueval else Falseval

notEquals :: Expr -> Expr -> Expr
notEquals (Integer' a) (Integer' b) = if a /= b then Trueval else Falseval
notEquals (Negative (Integer' a)) (Negative (Integer' b)) = if a /= b then Trueval else Falseval
notEquals (Negative (Integer' a)) (Integer' b) = if a == 0 && b == 0 then Falseval else Trueval
notEquals (Integer' a) (Negative (Integer' b)) = if a == 0 && b == 0 then Falseval else Trueval

greater :: Expr -> Expr -> Expr
greater (Integer' a) (Integer' b) = if a > b then Trueval else Falseval
greater (Negative (Integer' a)) (Negative (Integer' b)) = if a > b then Falseval else Trueval
greater (Negative (Integer' a)) (Integer' b) = Falseval
greater (Integer' a) (Negative (Integer' b)) = Trueval

greatEqual :: Expr -> Expr -> Expr
greatEqual (Integer' a) (Integer' b) = if a >= b then Trueval else Falseval
greatEqual (Negative (Integer' a)) (Negative (Integer' b)) = if a >= b then Falseval else Trueval
greatEqual (Negative (Integer' a)) (Integer' b) = Falseval
greatEqual (Integer' a) (Negative (Integer' b)) = Trueval

lesser :: Expr -> Expr -> Expr
lesser (Integer' a) (Integer' b) = if a < b then Trueval else Falseval
lesser (Negative (Integer' a)) (Negative (Integer' b)) = if a < b then Falseval else Trueval
lesser (Negative (Integer' a)) (Integer' b) = Trueval
lesser (Integer' a) (Negative (Integer' b)) = Falseval

--Care, special case of negative 0 and negative 0 as a and b respectively
lessEqual :: Expr -> Expr -> Expr
lessEqual (Integer' a) (Integer' b) = if a <= b then Trueval else Falseval
lessEqual (Negative (Integer' a)) (Negative (Integer' b)) = if a <= b then Falseval else Trueval
lessEqual (Negative (Integer' a)) (Integer' b) = Trueval
lessEqual (Integer' a) (Negative (Integer' b)) = Falseval


--make these exhaustive?
--
add :: Expr -> Expr -> Expr
add (Integer' a) (Integer' b) = Integer' (a + b)
add (Negative (Integer' a)) (Negative (Integer' b)) = Integer' (a + b)
add (Negative (Integer' a)) (Integer' b) = Integer' ((-a) + b)
add (Integer' a) (Negative (Integer' b)) = Integer' (a + (-b))


minus :: Expr -> Expr -> Expr
minus (Integer' a) (Integer' b) = Integer' (a - b)
minus (Negative (Integer' a)) (Negative (Integer' b)) = Integer' (a - b)
minus (Negative (Integer' a)) (Integer' b) = Integer' ((-a) - b)
minus (Integer' a) (Negative (Integer' b)) = Integer' (a - (-b))


divide :: Expr -> Expr -> Expr
divide (Integer' a) (Integer' b) = Integer' (div a  b)
divide (Negative (Integer' a)) (Negative (Integer' b)) = Integer' (div a b)
divide (Negative (Integer' a)) (Integer' b) = Integer' (div (-a) b)
divide (Integer' a) (Negative (Integer' b)) = Integer' (div a (-b))



mult :: Expr -> Expr -> Expr
mult (Integer' a) (Integer' b) = Integer' (a * b)
mult (Negative (Integer' a)) (Negative (Integer' b)) = Integer' (a * b)
mult (Negative (Integer' a)) (Integer' b) = Integer' ((-a) * b)
mult (Integer' a) (Negative (Integer' b)) = Integer' (a * (-b))

power :: Expr -> Expr -> Expr
power (Integer' a) (Integer' b) = Integer' (a ^ b)
power (Negative (Integer' a)) (Negative (Integer' b)) = Integer' (div 1 ((-a) ^ b))
power (Integer' a) (Negative (Integer' b)) = Integer' (div 1 (a^b))
power (Negative (Integer' a)) (Integer' b) = Integer' ((-a) ^ b)

--if i see nothing after print value, then NewLine
--if semicolong then space
--if comma, then tab
--parse each expr seperated by atleast one space
--check last value parsed
--if it is semicolon or comma or newline then let it be
--otherwise attach Newline
parsePrint :: Parser Stmt
parsePrint = do
              string "PRINT"
              spaces
              x <- option [Newline] (sepEndBy1 (parseExpr) spaces)
              case (last x) of
                Comma -> return $ PRINT x
                Semicolon -> return $ PRINT x
                Newline -> return $ PRINT x
                _ -> return $ PRINT (x++[Newline])

parseEnd :: Parser Stmt
parseEnd = do
            string "END"
            return END

parseGoto :: Parser Stmt
parseGoto = do
              string "GOTO"
              spaces
              num <- parseInteger
              return $ GOTO num
      --  liftM (GOTO . read) $ many1 digit

parseGosub :: Parser Stmt
parseGosub = do
              string "GOSUB"
              spaces
              num <- parseInteger
              return $ GOSUB num

parseReturn :: Parser Stmt
parseReturn = do
              string "RETURN"
              return RETURN

parseIfGoto :: Parser Stmt
parseIfGoto = do
                num <- parseInteger
                return $ GOTO num
--spaces :: Parser ()
--spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseLet :: Parser Stmt
parseLet = do
            string "LET"
            spaces
            var <- parseId
            spaces
            symbol
            spaces
            expr <- parseExpr
            return $ LET (var) expr


--FOR {index :: Expr, start :: Expr , stop :: Expr, step :: Expr}
--30 FOR I = 1 TO H
parseFor :: Parser Stmt
parseFor = do
            string "FOR"
            spaces
            index <- parseId
            spaces
            char '='
            spaces
            start <- try parseInt <|> parseId
            spaces
            string "TO"
            spaces
            stop <- parseInt <|> parseId
            spaces
            step <- option (Integer' 1) $ do {string "STEP"; spaces; parseInt}
            return $ FOR index start stop step


--TODO change goto so it is just arbitrary statement.  Alt is the else part of If
parseIf :: Parser Stmt
parseIf = do
            string "IF"
            spaces
            test <- parseOpEquality
            spaces
            string "THEN"
            spaces
            c <- parseIfGoto <|> parseStmt
            return $ IF test [c] []

--INPUT {prompt :: String, vars :: [Expr]}
--"does not work for promtps"
parseInput :: Parser Stmt
parseInput = do
              string "INPUT"
              spaces
              prompt <- do {(char '"'); manyTill anyChar (try $ string "\";")}
              spaces
              a <- sepBy parseId (char ';')
              return $ INPUT prompt a

parseNoPromptInput :: Parser Stmt
parseNoPromptInput = do
              string "INPUT"
              spaces
              a <- many1 parseId
              return $ INPUT "" a
--10 NEXT J, I
--TODO needs to be able to parse multiple Ids
--can use sepBy comma
parseNext :: Parser Stmt
parseNext = do
              string "NEXT"
              spaces
              x <- parseId
              return $ NEXT [x]



parseStmt :: Parser Stmt
parseStmt = parseLet <|> try parseGoto <|> try parseGosub <|> parseReturn <|> parsePrint <|> parseEnd <|> parseNext
              <|> try parseInput <|> try parseNoPromptInput <|> try parseIf <|> parseFor


--this can add to the list rather than just returning one pair value
parseLineAndStmt :: Parser [(Int, Stmt)]
parseLineAndStmt = do
              a <- parseInteger
              spaces
              --instead just parseStmt on each string seperated by :
              b <- sepEndBy parseStmt (do {spaces; char ':'; spaces})
              return $ fmap (\x-> (a,x)) b


parseInteger :: Parser Int
parseInteger = read <$> many1 digit

parseInt :: Parser Expr
parseInt = liftM (Integer' . read) $ many1 digit

parseString :: Parser Expr
parseString  = do
                prompt <- do {(char '"'); manyTill anyChar (try $ char '"')}
                return $ String' prompt

parseNegative :: Parser Expr
parseNegative = do
                char '-'
                a <- parseExpr
                return $ Negative a

equalitySymbol :: Parser Char
equalitySymbol = oneOf "<=>"

parseOpEquality :: Parser Expr
parseOpEquality = do
          first <- try parseOpHigh <|> try parseOp <|> try parseGrouped <|> try parseFunction <|> try parseInt <|> parseId <|> parseNegative
          spaces
          operator <- many1 equalitySymbol
          spaces
          second <- try parseOpHigh <|> try parseOp <|> try parseGrouped <|> try parseFunction <|> try parseInt <|> parseId <|> parseNegative
          case operator of
            "=" -> return $ Op "=" equals first second
            ">=" -> return $ Op ">=" greatEqual first second
            "<=" -> return $ Op "<=" lessEqual first second
            "<>" -> return $ Op "<>" notEquals first second
            ">" -> return $ Op ">" greater first second
            "<" -> return $ Op "<" lesser first second

--TODO Known bug, doesnt do operations in correct order correctly
parseOpHigh :: Parser Expr
parseOpHigh = do
          first <- try parseId <|> try parseGrouped <|> try parseFunction <|> try parseInt <|>  parseNegative
          spaces
          operator <- oneOf "/*^"
          spaces
          second <- try parseOpHigh <|> try parseGrouped <|> try parseFunction <|> try parseInt <|> parseId <|> parseNegative
          case operator of
            '/' -> return $ Op "/" divide first second
            '*' -> return $ Op "*" mult first second
            '^' -> return $ Op "^" power first second

parseOp :: Parser Expr
parseOp = do
          first <- try parseOpHigh <|> try parseId <|> try parseGrouped <|> try parseFunction <|> try parseInt <|>  parseNegative
          spaces
          operator <- oneOf "+-"
          spaces
          second <- try parseOpHigh <|> try parseOp <|> try parseGrouped <|> try parseFunction <|> try parseInt <|> parseId <|> parseNegative
          case operator of
            '+' -> return $ Op "+" add first second
            '-' -> return $ Op "-" minus first second




parseId :: Parser Expr
parseId = do
            id <- letter
            return $ Id id
--liftM (Id . read) $ many1 letter

parseGrouped :: Parser Expr
parseGrouped = do
                char '('
                x <- parseExpr
                char ')'
                return $ Grouped x



--25 LET X = INT(RND(1)*H+1)
--INT(RND(1)*H+1)
--Function String Expr
--TODO perhaps there is better way to parse funtion word before parens
parseFunction :: Parser Expr
parseFunction = do
                fun <- (string "INT") <|> (string "RND") <|> (string "DIM")
                spaces
                a <- parseExpr
                return $ Function fun a

parseDimensions :: Parser Expr
parseDimensions = do
                    arrName <- parseId
                    spaces
                    char '('
                    arrSize <- parseInteger
                    char ')'
                    return $ Dimensions arrName arrSize

parseSemicolon :: Parser Expr
parseSemicolon = do
                  char ';'
                  return Semicolon

parseComma :: Parser Expr
parseComma = do
              char ','
              return Comma



parseExpr :: Parser Expr
parseExpr = do
              x <- try parseOpEquality <|> try parseOp <|> try parseOpHigh <|> try parseGrouped
                    <|>  try parseFunction <|> try parseDimensions <|> parseInt <|> parseId <|> parseNegative
                    <|> parseString <|> parseComma <|> parseSemicolon
              return x




--updateLet :: Expr -> Expr -> [(Char, Expr)] -> [(Char, Expr)] -> [(Char, Expr)]
--updateLet v@(Id x) y [] ts = ts
--updateLet v@(Id x) y (z:zs) ts = if (fst z) == x then updateLet v y zs ([(x, (evaluateExpr y))] ++ ts) else updateLet v y zs (ts ++ [z])

--TODO fix so that i update as well
createLetvalues :: [(Int, Stmt)] -> [(Char, Expr)] -> [(Char, Expr)]
createLetvalues [] ys = ys
createLetvalues (x:xs) ys = case snd x of
                              LET (Id charVal) expr2 -> do
                                                          --let y = getvalue expr2 ys
                                                          createLetvalues xs (ys ++ [(charVal, expr2)])
                              _ -> createLetvalues xs ys


--evalGosubReturn =

--TODO POSSIBLE CASE what if IF has multiple operations instead of just Id or Id Integer' type stuff
evalForLoop :: Int -> Expr -> Int -> Int -> Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [IOArray Int Expr] -> [(Int, Stmt)] -> IO (Cont,String)
evalForLoop lineNumber index n stepInt stop loop ys arrays oParses = if n /= (stop+1) then do
                                                                                  let i = (lineNumber, (LET index (Integer' n)))
                                                                                  (newYs, evalEnd) <- eval (i:loop) ys arrays oParses
                                                                                  if (equalEndStmt evalEnd STOPEVAL) then return (STOPEVAL, "")
                                                                                    else evalForLoop lineNumber index (n+stepInt) stepInt stop loop (newYs) arrays oParses
                                                                               else return $ (CONTINUEEVAL,(show (n-1)))
--make so able to move to goto outside for loop
--the first is the loop the second is the rest
findNextFromParsedList :: [(Int, Stmt)] -> [(Int, Stmt)] -> ([(Int, Stmt)],[(Int, Stmt)])
findNextFromParsedList [] ys = (ys,[]) --if this occurs then needs to  error
findNextFromParsedList (x:xs) ys = case (snd x) of
                                      NEXT _ -> (ys,xs) -- have to update with new let value which is n
                                      _ -> findNextFromParsedList xs (ys++[x])

findReturnFromParsedList :: [(Int, Stmt)] -> [(Int, Stmt)] -> [(Int, Stmt)]
findReturnFromParsedList [] ys = (ys) --if this occurs then needs to  error
findReturnFromParsedList (x:xs) ys = case (snd x) of
                                      RETURN -> ys -- have to update with new let value which is n
                                      _ -> findReturnFromParsedList xs (ys++[x])
--TODO NEED TO get gosub working
data Cont = CONTINUEEVAL | STOPEVAL
--Dimensions {name :: Id, size :: Int}
--TODO try and get my state working correctly
--otherwise just pass a list of array values to use
--then try and get state working later
eval :: [(Int, Stmt)] -> [(Int, Stmt)] -> [IOArray Int Expr] -> [(Int, Stmt)] -> IO ([(Int, Stmt)], Cont)
eval [] ys _ _ = return (ys, CONTINUEEVAL)
eval (x:xs) ys arrays oParses = do
                          --putStrLn ("evaluating currrently " ++ show (snd x))
                          case (snd x) of
                            LET (Dimensions arrName index) expr2 -> do
                                                                      let letValues = createLetvalues ys []
                                                                      --arr <- get
                                                                      val <- getvalue expr2 letValues arrays
                                                                      --writeArray arr index val
                                                                      --put arr
                                                                      eval xs (ys) arrays oParses
                            LET expr1 expr2 -> do
                                                let letValues = createLetvalues ys []
                                                val <- getvalue expr2 letValues arrays
                                                let z = (fst x, LET expr1 val)
                                                eval xs (ys++[z]) arrays oParses
                            FOR index@(Id _) start@(_) stop@(_) step@(Integer' stepval) -> do
                                                                                            let letValues = createLetvalues ys []
                                                                                            startEvaluated <- (getvalue start letValues arrays)
                                                                                            stopEvaluated <- (getvalue stop letValues arrays)
                                                                                            let (loop, rest) = findNextFromParsedList xs []
                                                                                            (continue ,endIndex) <- evalForLoop (fst x) index (integer' startEvaluated) stepval (integer' stopEvaluated) loop ys arrays oParses
                                                                                            if (equalEndStmt continue STOPEVAL) then eval [] [] [] []
                                                                                            else do
                                                                                                  let y = ((fst x), (LET index (Integer' (read endIndex :: Int))))
                                                                                                  eval rest (ys ++ [y]) arrays oParses
                            INPUT prompt a -> do
                                          --needs to be ablt to run this block on each expr element
--                                          putStrLn "INput is being evaled "
                                          putStr prompt
                                          val <- getLine
                                          let valParsed = Integer' (read val :: Int)
                                          let listOfLetValues = makeLetVals (fst x) a valParsed []
                                          --TODO take notice removed ++ [x] ++ from below ys ++listOfLetValues....I dont think its needed to retain input statment
                                          eval xs (ys ++ listOfLetValues) arrays oParses
                            IF test conseqStmt [] -> do
                                                          let letValues = createLetvalues ys []
                                                          b <- getvalue test letValues arrays
                                                          if (getBoolValue b)
                                                            then eval ([((fst x), head conseqStmt)]) (ys++[x]) arrays oParses
                                                              else eval xs (ys++[x]) arrays oParses
                            GOTO lineNumber -> evalGoto lineNumber oParses ys arrays oParses
                            GOSUB lineNumber -> evalGosub lineNumber oParses ys xs arrays oParses
                            RETURN -> eval xs (ys++[x]) arrays oParses
                                                  --need to find the list of statements starting from the linenumber
                            --TODO fix print for multiple exprs i guess
                            PRINT exprvalues -> do
                                                  let letValues = createLetvalues ys []
                                                  makeAndPrintStmts exprvalues letValues arrays
                                                  --printStatement <-  evalprint exprvalues letValues
                                                  --print printStatement
                                                  eval xs (ys++[x]) arrays oParses
                            END -> return (ys, STOPEVAL)
                            _ -> do
                                  putStrLn "Error in evaluation"
                                  return (ys, STOPEVAL)

--BASIC uses the PRINT statement to print things.
--The PRINT statement has a strange syntax.  PRINT is followed by a list of
--zero or more expressions separated by commas or semicolons.
--After printing an expression followed by a comma,
--the printhead is advanced to the next tab stop;
--after printing an expression followed by a semicolon, the printhead is advanced to the next character.
-- The final expression in the list may be followed by a comma, a semicolon or nothing;
--if it is followed by nothing, a carriage return and linefeed are printed.

--if i see nothing after print value, then NewLine
--if semicolong then space
--if comma, then tab

makeAndPrintStmts :: [Expr] -> [(Char, Expr)] -> [IOArray Int Expr] -> IO ()
makeAndPrintStmts [] _ _ = putStr ""
makeAndPrintStmts (x:xs) letValues arrays = do
                                        printStatement <- (getvalue x letValues arrays)
                                        putStr $ show printStatement
                                        --print printStatement
                                        makeAndPrintStmts xs letValues arrays


makeLetVals :: Int -> [Expr] -> Expr -> [(Int, Stmt)] -> [(Int, Stmt)]
makeLetVals lineNumber [] value ys = ys
makeLetVals lineNumber (x:xs) value ys = makeLetVals lineNumber xs value (ys ++ [(lineNumber, LET x value)])

--need to test, i think may have problem with loop and rest being evaluated correctly
--want to create a list containing all the elements from gosub line onward till return
evalGosub :: Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [(Int, Stmt)] -> [IOArray Int Expr] -> [(Int, Stmt)] ->IO ([(Int, Stmt)], Cont)
evalGosub linenumber (x:xs) ys zs arrays oParses = if (fst x) == linenumber
                                          then do
                                            let gosubStmts = findReturnFromParsedList (x:xs) []
                                            (newYs, _) <- eval gosubStmts ys arrays oParses
                                            eval (zs) (newYs) arrays oParses
                                          else evalGosub linenumber xs ys zs arrays oParses


--this works except needs to stop for loop
evalGoto :: Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [IOArray Int Expr] -> [(Int, Stmt)] -> IO ([(Int, Stmt)],Cont)
evalGoto lineNumber (x:xs) ys arrays oParses = if (fst x) == lineNumber
                                          then (eval (x:xs) ys arrays oParses)
                                          else evalGoto lineNumber xs ys arrays oParses

--data Expr = Op String (Expr -> Expr -> Expr) Expr Expr | Negative Expr | Grouped Expr | Function String Expr | Id {char' :: Char} | Integer' {integer' :: Int} | String' String | Error String | Semicolon
--  (Op _ operation expr1 expr2)
--TODO may combine this with getvalue
--evalprint :: [Expr] -> [(Char, Expr)] -> IO Expr
--evalprint (x:xs) ys = case x of
                        --expr@(Id a) -> (getvalue expr ys)
                        --Op oper operation expr1 expr2 -> do
                          --                                a <- getvalue expr1 ys
                            --                              b <- getvalue expr2 ys
                              --                            return $ operation a b
  --                      a -> (getvalue a ys)
                      --TODO just match oper with some function that will operate on two values

--little surprised this works in evaluation with float
rand :: Expr -> IO Expr
rand (Integer' val) = do
              g <- newStdGen
              if val == 1 then return $ Float' $ fst (randomR(0, 1.0) g)
                else return $ Integer' $ fst (randomR(0 ,val) g)

--evalFunction :: String -> Expr -> Expr
--evalFunction name (Integer' val) = case name of
--                                      "INT" -> Integer' (round val)
--                                      "RND" -> rand val

getBoolValue :: Expr -> Bool
getBoolValue Trueval = True
getBoolValue Falseval = False

equalEndStmt :: Cont -> Cont -> Bool
equalEndStmt x y = case (x,y) of
                    (STOPEVAL, STOPEVAL) -> True
                    _ -> False


--liftM :: Monad m => (a1 -> r) -> m a1 -> m r
--TODO need to get order of operations working correctly and input assigning to value correctly
getvalue :: Expr -> [(Char, Expr)] -> [IOArray Int Expr] -> IO Expr
getvalue x ys arrays = do
                  --value <- x
                  case (x, ys) of
                    (v@(Integer' _), _) -> return v
                    (Id a, []) -> return (Error ("Found no value for " ++ [a]))
                    (Id a, ys) -> return $ snd $ last $ filter (\z -> if (fst z) == a then True else False) ys
                    ((Op o operation a b), ys) -> liftM2 operation (getvalue a ys arrays) (getvalue b ys arrays)
                    ((Grouped (a)), ys) -> getvalue a ys arrays
                    ((Negative a), ys) -> do
                                            ioVal <- getvalue a ys arrays
                                            return $ Negative (ioVal)
                    ((Function name val), ys) -> case name of
                                                        "INT" -> do
                                                                  newVal <- (getvalue val ys arrays)
                                                                  return $ newVal
                                                        "RND" -> do
                                                                  newVal <- (getvalue val ys arrays )
                                                                  rand newVal
                                                        --this will declare the actual array and store it for use
                                                        --will have to write instance eq for Integer'
                                                        "DIM" -> do
                                                                  --get existing list of arrays
                                                                  --then put a new array with that
                                                                  --put (newArray_ (1, (size val)) :: IO (IOArray Int Expr))
                                                                  return (Integer' 0)
                    ((Dimensions arrayName index), ys) -> do --will have to match the array
                                                            --arr <- get
                                                            --val <- readArray arr index
                                                            --return val

                                                            return (Integer' 0)
                      --this will return the value of the element at the index for the specified array
                    (a@(String' _), ys) -> return a
                    (x,ys) -> return x


--each line will start with number in begining denoting line number
--any other command will have something before it
readStmt :: String -> [(Int, Stmt)]
readStmt input = case parse (parseLineAndStmt) "BASIC" input of
  Left err -> [(0, REM $ "No match: " ++ show err)]
  Right val -> val

--parseLines :: [String] -> [(Expr, Stmt)]
parseLines [] ys = ys
parseLines xs ys = parseLines (tail xs) (ys ++ (readStmt (head xs)))

  --p :: FilePath -> [(Char, Expr)] -> [(Expr, Stmt)] -> IO ()
  --[(Expr, Stmt)]
p stmt = do
          contents <- (readFile stmt)
          let linesOfBasic = lines contents
          return $ parseLines linesOfBasic []
          --TODO delete above line and uncomment below two lines for evaluation
          --let parsed = (parseLines linesOfBasic [])
          --eval parsed [] parsed
peval stmt = do
          contents <- (readFile stmt)
          let linesOfBasic = lines contents
          let parsed = parseLines linesOfBasic []
          eval parsed [] [] parsed

--main :: IO ()
--main = getArgs >>= print . eval . readStmt . head
