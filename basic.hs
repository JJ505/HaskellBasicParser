--Jalen Johnson
import Text.ParserCombinators.Parsec
-- hiding (spaces)
import System.Environment
import Control.Monad
import System.Random
import Control.Monad.Trans


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
data Expr = Op String (Expr -> Expr -> Expr) Expr Expr | Negative Expr | Grouped Expr | Function String Expr | Id {char' :: Char} | Integer' {integer' :: Int} | Float' {float' :: Float}| String' String | Error String | Semicolon | Comma | Newline | Trueval | Falseval | Dimensions {name :: Expr, size :: Expr}

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
    show (Dimensions n i) = (show n) ++ "(" ++ (show i) ++ ")"


data Stmt = REM String | FOR {index :: Expr, start :: Expr , stop :: Expr, step :: Expr} | IF {test :: Expr, conseq :: [Stmt], alt :: [Stmt]} | PRINT [Expr] | GOTO {line :: Int} | GOSUB {line :: Int} | INPUT {prompt :: String, vars :: [Expr]} | LET {lhs :: Expr, rhs :: Expr} | NEXT {indices' :: [Expr]} | DIM {arr :: Expr} | RETURN | END

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
    show (DIM d) = "DIM " ++ show d
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

lessEqual :: Expr -> Expr -> Expr
lessEqual (Integer' a) (Integer' b) = if a <= b then Trueval else Falseval
lessEqual (Negative (Integer' a)) (Negative (Integer' b)) = if a <= b then Falseval else Trueval
lessEqual (Negative (Integer' a)) (Integer' b) = Trueval
lessEqual (Integer' a) (Negative (Integer' b)) = Falseval


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

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseLet :: Parser Stmt
parseLet = do
            string "LET"
            spaces
            var <- try parseDimensions <|> try parseId
            spaces
            symbol
            spaces
            expr <- parseExpr
            return $ LET (var) expr


parseFor :: Parser Stmt
parseFor = do
            string "FOR"
            spaces
            index <- parseId
            spaces
            char '='
            spaces
            start <- try parseInt <|> try parseId <|> try parseGrouped
            spaces
            string "TO"
            spaces
            stop <- try parseOpHigh <|> try parseOp <|> try parseInt
                    <|> try parseId <|> try parseGrouped
            spaces
            step <- option (Integer' 1) $ do {string "STEP"; spaces;
                                              try parseInt
                                              <|> try parseNegative}
            return $ FOR index start stop step


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

parseInput :: Parser Stmt
parseInput = do
              string "INPUT"
              spaces
              prompt <- do {(char '"'); manyTill anyChar (try $ string "\";")}
              spaces
              a <- sepBy parseId (char ',')
              return $ INPUT prompt a

parseNoPromptInput :: Parser Stmt
parseNoPromptInput = do
              string "INPUT"
              spaces
              a <- many1 parseId
              return $ INPUT "" a

parseNext :: Parser Stmt
parseNext = do
              string "NEXT"
              spaces
              x <- sepEndBy parseId (do {spaces; char ','; spaces})
              return $ NEXT x

parseDim :: Parser Stmt
parseDim = do
            string "DIM"
            spaces
            a <- parseDimensions
            return $ DIM a

parseRem :: Parser Stmt
parseRem = do
            string "REM"
            spaces
            a <- many1 anyChar
            return $ REM a

parseStmt :: Parser Stmt
parseStmt = parseLet <|> try parseGoto <|> try parseGosub <|> try parseDim <|> try parseReturn
              <|> try parsePrint <|> try parseEnd <|> try parseNext <|> try parseInput
              <|> try parseNoPromptInput <|> try parseIf <|> try parseFor <|> try parseRem


parseLineAndStmt :: Parser [(Int, Stmt)]
parseLineAndStmt = do
              a <- parseInteger
              spaces
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
          first <- try parseOpHigh <|> try parseOp <|> try parseGrouped
                    <|> try parseDimensions <|> try parseFunction
                    <|> try parseInt <|> parseId <|> parseNegative
          spaces
          operator <- many1 equalitySymbol
          spaces
          second <- try parseOpHigh <|> try parseOp <|> try parseGrouped
                    <|> try parseDimensions <|> try parseFunction
                    <|> try parseInt <|> parseId <|> parseNegative
          case operator of
            "=" -> return $ Op "=" equals first second
            ">=" -> return $ Op ">=" greatEqual first second
            "<=" -> return $ Op "<=" lessEqual first second
            "<>" -> return $ Op "<>" notEquals first second
            ">" -> return $ Op ">" greater first second
            "<" -> return $ Op "<" lesser first second

parseOpHigh :: Parser Expr
parseOpHigh = do
          first <- try parseId <|> try parseGrouped <|> try parseDimensions
                    <|> try parseFunction <|> try parseInt <|>  parseNegative
          spaces
          operator <- oneOf "/*^"
          spaces
          second <- try parseOpHigh <|> try parseGrouped <|> try parseDimensions
                     <|> try parseFunction <|> try parseInt <|> parseId <|> parseNegative
          case operator of
            '/' -> return $ Op "/" divide first second
            '*' -> return $ Op "*" mult first second
            '^' -> return $ Op "^" power first second

parseOp :: Parser Expr
parseOp = do
          first <- try parseOpHigh <|> try parseDimensions <|> try parseId
                    <|> try parseGrouped <|> try parseFunction
                    <|> try parseInt <|>  parseNegative
          spaces
          operator <- oneOf "+-"
          spaces
          second <- try parseOpHigh <|> try parseOp <|> try parseDimensions
                    <|> try parseGrouped <|> try parseFunction <|> try parseInt
                    <|> parseId <|> parseNegative
          case operator of
            '+' -> return $ Op "+" add first second
            '-' -> return $ Op "-" minus first second

parseId :: Parser Expr
parseId = do
            id <- letter
            return $ Id id

parseGrouped :: Parser Expr
parseGrouped = do
                char '('
                x <- parseExpr
                char ')'
                return $ Grouped x

parseFunction :: Parser Expr
parseFunction = do
                fun <- (string "INT") <|> (string "RND")
                spaces
                a <- parseExpr
                return $ Function fun a

parseDimensions :: Parser Expr
parseDimensions = do
                    arrName <- parseId
                    spaces
                    char '('
                    arrSize <- parseExpr
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
                    <|>  try parseFunction <|> try parseDimensions <|> try parseInt
                    <|> try parseId <|> try parseNegative <|> try parseString <|> try parseComma
                    <|> try parseSemicolon
              return x

createLetvalues :: [(Int, Stmt)] -> [(Char, Expr)] -> [(Char, Expr)]
createLetvalues [] ys = ys
createLetvalues (x:xs) ys = case snd x of
                              LET (Id charVal) expr2 -> do
                                  createLetvalues xs (ys ++ [(charVal, expr2)])
                              _ -> createLetvalues xs ys


evalForLoop :: Int -> Expr -> Int -> Int -> Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [((Char, Int), [(Int, Expr)])] -> [(Int, Stmt)] -> IO (Cont,String, [((Char, Int), [(Int, Expr)])])
evalForLoop lineNumber index n stepInt stop loop ys arrays oParses = if n /= (stop+stepInt) then do
                                                                                  let i = (lineNumber, (LET index (Integer' n)))
                                                                                  (newYs, newArs, evalEnd) <- eval (i:loop) ys arrays oParses
                                                                                  if (equalEndStmt evalEnd STOPEVAL) then return (STOPEVAL, "", newArs)
                                                                                    else evalForLoop lineNumber index (n+stepInt) stepInt stop loop (newYs) newArs oParses
                                                                               else return $ (CONTINUEEVAL,(show (n-stepInt)), arrays)

checkNextList :: Char -> [Expr] -> Bool
checkNextList c [] = False
checkNextList c (x:xs) = if (char' x) == c then True else checkNextList c xs

findNextFromParsedList :: Char -> [(Int, Stmt)] -> [(Int, Stmt)] -> ([(Int, Stmt)],[(Int, Stmt)])
findNextFromParsedList c [] ys = (ys,[])
findNextFromParsedList c (x:xs) ys = case (snd x) of
                                      NEXT a -> if checkNextList c a then (ys,xs) else findNextFromParsedList c xs (ys++[x])
                                      _ -> findNextFromParsedList c xs (ys++[x])

findReturnFromParsedList :: [(Int, Stmt)] -> [(Int, Stmt)] -> [(Int, Stmt)]
findReturnFromParsedList [] ys = (ys)
findReturnFromParsedList (x:xs) ys = case (snd x) of
                                      RETURN -> ys
                                      _ -> findReturnFromParsedList xs (ys++[x])
data Cont = CONTINUEEVAL | STOPEVAL
eval :: [(Int, Stmt)] -> [(Int, Stmt)] -> [((Char, Int), [(Int, Expr)])] -> [(Int, Stmt)] ->  IO ([(Int, Stmt)], [((Char, Int), [(Int, Expr)])], Cont)
eval [] ys arrays _ = return (ys, arrays, CONTINUEEVAL)
eval (x:xs) ys arrays oParses = do
              case (snd x) of
                REM _ -> eval xs ys arrays oParses
                DIM (Dimensions (Id name) size) -> do
                              let letValues = createLetvalues ys []
                              intSize <- getvalue size letValues arrays
                              eval xs ys (arrays ++ [((name, (integer' intSize)), [])]) oParses
                LET (Dimensions (Id arrName) index) expr2 -> do
                                          let letValues = createLetvalues ys []
                                          val <- getvalue expr2 letValues arrays
                                          i <- getvalue index letValues arrays
                                          let newArrays = removeReplicasFromArrays (map (\z -> if ((fst(fst z)) == arrName) && ((snd (fst z)) >= (integer' i)) then ((fst z), ((snd z)++[((integer' i), val)])) else z) arrays)
                                          eval xs ys newArrays oParses
                LET expr1 expr2 -> do
                                    let letValues = createLetvalues ys []
                                    val <- getvalue expr2 letValues arrays
                                    let z = (fst x, LET expr1 val)
                                    eval xs (ys++[z]) arrays oParses
                FOR index@(Id c) start@(_) stop@(_) step@(Integer' stepval) -> do
                                                        let letValues = createLetvalues ys []
                                                        startEvaluated <- (getvalue start letValues arrays)
                                                        stopEvaluated <- (getvalue stop letValues arrays)
                                                        let (loop, rest) = findNextFromParsedList c xs []
                                                        (continue ,endIndex, newArs) <- evalForLoop (fst x) index (integer' startEvaluated) stepval (integer' stopEvaluated) loop ys arrays oParses
                                                        if (equalEndStmt continue STOPEVAL) then eval [] [] [] []
                                                        else do
                                                              let y = ((fst x), (LET index (Integer' (read endIndex :: Int))))
                                                              eval rest (ys ++ [y]) newArs oParses
                FOR index@(Id c) start@(_) stop@(_) step@(Negative (Integer' stepval)) -> do
                                                        let letValues = createLetvalues ys []
                                                        startEvaluated <- (getvalue start letValues arrays)
                                                        stopEvaluated <- (getvalue stop letValues arrays)
                                                        let (loop, rest) = findNextFromParsedList c xs []
                                                        (continue ,endIndex, newArs) <- evalForLoop (fst x) index (integer' startEvaluated) ((-1) * stepval) (integer' stopEvaluated) loop ys arrays oParses
                                                        if (equalEndStmt continue STOPEVAL) then eval [] [] [] []
                                                        else do
                                                              let y = ((fst x), (LET index (Integer' (read endIndex :: Int))))
                                                              eval rest (ys ++ [y]) newArs oParses
                INPUT prompt a -> do
                              putStr prompt
                              val <- getLine
                              let valParsed = Integer' (read val :: Int)
                              let listOfLetValues = makeLetVals (fst x) a valParsed []
                              eval xs (ys ++ listOfLetValues) arrays oParses
                IF test conseqStmt [] -> do
                                              let letValues = createLetvalues ys []
                                              b <- getvalue test letValues arrays
                                              if (getBoolValue b)
                                                then eval ([((fst x), head conseqStmt)]) (ys++[x]) arrays oParses
                                                  else eval xs (ys++[x]) arrays oParses
                GOTO lineNumber -> evalGoto lineNumber oParses ys arrays oParses xs
                GOSUB lineNumber -> evalGosub lineNumber oParses ys xs arrays oParses
                RETURN -> eval xs (ys++[x]) arrays oParses
                PRINT exprvalues -> do
                                      let letValues = createLetvalues ys []
                                      makeAndPrintStmts exprvalues letValues arrays
                                      eval xs (ys++[x]) arrays oParses
                END -> return (ys, arrays, STOPEVAL)
                _ -> do
                      putStrLn "Error in evaluation"
                      return (ys, arrays, STOPEVAL)

makeAndPrintStmts :: [Expr] -> [(Char, Expr)] -> [((Char, Int), [(Int, Expr)])] -> IO ()
makeAndPrintStmts [] _ _ = putStr ""
makeAndPrintStmts (x:xs) letValues arrays = do
                                        printStatement <- (getvalue x letValues arrays)
                                        putStr $ show printStatement
                                        makeAndPrintStmts xs letValues arrays


makeLetVals :: Int -> [Expr] -> Expr -> [(Int, Stmt)] -> [(Int, Stmt)]
makeLetVals lineNumber [] value ys = ys
makeLetVals lineNumber (x:xs) value ys = makeLetVals lineNumber xs value (ys ++ [(lineNumber, LET x value)])

evalGosub :: Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [(Int, Stmt)] -> [((Char, Int), [(Int, Expr)])] -> [(Int, Stmt)] ->IO ([(Int, Stmt)],[((Char, Int), [(Int, Expr)])], Cont)
evalGosub linenumber (x:xs) ys zs arrays oParses = if (fst x) == linenumber
                                          then do
                                            let gosubStmts = findReturnFromParsedList (x:xs) []
                                            (newYs,newArs, _) <- eval gosubStmts ys arrays oParses
                                            eval (zs) (newYs) newArs oParses
                                          else evalGosub linenumber xs ys zs arrays oParses

evalGoto :: Int -> [(Int, Stmt)] -> [(Int, Stmt)] -> [((Char, Int), [(Int, Expr)])] -> [(Int, Stmt)] -> [(Int, Stmt)] -> IO ([(Int, Stmt)],[((Char, Int), [(Int, Expr)])],Cont)
evalGoto lineNumber (x:xs) ys arrays oParses originalXs = if (fst x) == lineNumber
                                          then case (snd x) of
                                                NEXT _ ->  eval originalXs ys arrays oParses
                                                _ -> (eval (x:xs) ys arrays oParses)
                                          else evalGoto lineNumber xs ys arrays oParses originalXs

rand :: Expr -> IO Expr
rand (Integer' val) = do
              g <- newStdGen
              if val == 1 then return $ Float' $ fst (randomR(0, 1.0) g)
                else return $ Integer' $ fst (randomR(0 ,val) g)

getBoolValue :: Expr -> Bool
getBoolValue Trueval = True
getBoolValue Falseval = False

equalEndStmt :: Cont -> Cont -> Bool
equalEndStmt x y = case (x,y) of
                    (STOPEVAL, STOPEVAL) -> True
                    _ -> False

removeReplicasFromArrays :: [((Char, Int), [(Int,Expr)])] -> [((Char, Int), [(Int,Expr)])]
removeReplicasFromArrays xs = map (\x-> ((fst x), removeReplicas (snd x) [])) xs

removeReplicas :: [(Int, Expr)] -> [(Int, Expr)] -> [(Int, Expr)]
removeReplicas [] ys = ys
removeReplicas xs@(x:vs) ys = removeReplicas (filter (\z-> if (fst x) == (fst z) then False else True) xs) (ys ++ [(last (filter (\z-> if (fst x) == (fst z) then True else False) xs))])

getvalue :: Expr -> [(Char, Expr)] -> [((Char, Int), [(Int,Expr)])] -> IO Expr
getvalue x ys arrays  = do
                  case (x, ys) of
                    (v@(Integer' _), _) -> return v
                    (Id a, []) -> return (Error ("Found no value for " ++ [a]))
                    (Id a, ys) -> return $ snd $ last $ filter (\z -> if (fst z) == a then True else False) ys
                    ((Op o operation a b), ys) -> liftM2 operation (getvalue a ys arrays ) (getvalue b ys arrays)
                    ((Grouped (a)), ys) -> getvalue a ys arrays
                    ((Negative a), ys) -> do
                                            ioVal <- getvalue a ys arrays
                                            return $ Negative (ioVal)
                    ((Function name val), ys) -> case name of
                                                        "INT" -> do
                                                                  newVal <- (getvalue val ys arrays)
                                                                  return $ newVal
                                                        "RND" -> do
                                                                  newVal <- (getvalue val ys arrays)
                                                                  rand newVal
                    ((Dimensions (Id arrayName) index), _) -> do
                                                i <- getvalue index ys arrays
                                                let list = snd $ head $ filter (\z -> if (fst(fst z)) == arrayName then True else False) arrays
                                                let val = snd $ last $ filter (\z -> if (integer' i) == (fst z) then True else False) ([(integer' i, (Integer' 0))]++list)
                                                return val
                    (a@(String' _), ys) -> return a
                    (x,ys) -> return x

readStmt :: String -> [(Int, Stmt)]
readStmt input = case parse (parseLineAndStmt) "BASIC" input of
  Left err -> [(0, REM $ "No match: " ++ show err)]
  Right val -> val

parseLines [] ys = ys
parseLines xs ys = parseLines (tail xs) (ys ++ (readStmt (head xs)))

p stmt = do
          contents <- (readFile stmt)
          let linesOfBasic = lines contents
          return $ parseLines linesOfBasic []

peval stmt = do
          contents <- (readFile stmt)
          let linesOfBasic = lines contents
          let parsed = parseLines linesOfBasic []
          eval parsed [] [] parsed
