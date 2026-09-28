{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Applicative (Alternative (..))
import Control.Monad (when)
import Control.Monad.State (State, execState, gets, modify)
import Data.List (nub)
import qualified Data.Map as Map
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment (getArgs)

-- Igor Stec 469565

-- Sekwencja z dodawaniem na koniec w czasie O(1) - elementy spbie trzymamy
-- wewnetrznie odwrocone, najnowszy na czele listy.
newtype SnocList a = SnocList {unSnocList :: [a]}

toList :: SnocList a -> [a]
toList (SnocList xs) = reverse xs

fromList :: [a] -> SnocList a
fromList xs = SnocList (reverse xs)

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

instance Eq a => Eq (SnocList a) where
  (==) :: Eq a => SnocList a -> SnocList a -> Bool
  s1 == s2 = toList s1 == toList s2

instance Show a => Show (SnocList a) where
  show :: Show a => SnocList a -> String
  show s = show (toList s)

instance Semigroup (SnocList a) where

  -- xs jest przed ys; oba odwrocone, wiec ys ++ xs bedize okej
  SnocList xs <> SnocList ys = SnocList (ys ++ xs)

instance Monoid (SnocList a) where
  mempty = SnocList []

instance Functor SnocList where
  fmap f (SnocList xs) = SnocList (map f xs)

instance Applicative SnocList where
  pure x = SnocList [x]
  SnocList fs <*> SnocList xs =
    fromList [f x | f <- reverse fs, x <- reverse xs]

instance Alternative SnocList where
  empty = mempty
  (<|>) = (<>)


-- Typy danych


type Name = String

-- Jedna klauzula definicji funkcji.
data Match = Match
  { matchName :: Name
  , matchPats :: [Pat]
  , matchRhs  :: Expr
  } deriving (Show)

-- Wszystkie klauzule danej nazwy.
newtype Def = Def {defMatches :: [Match]} deriving (Show)

infixl 9 :$

data Expr
  = Var Name     -- nazwa z malej litery
  | Con Name     -- konstruktor
  | Expr :$ Expr -- aplikacja
  deriving (Show, Eq)

-- argumenty Pat maja teraz rozne wyglady:
-- PVar Name — wzorzec-zmienna
-- PApp Name [Pat] - wzorzec-konstruktor. Pasuje tylko wtedy, gdy wyrażenie ma na wierzchu ten konkretny konstruktor.
-- Z -> PApp "Z" [],            S(S z) -> PApp "S" [ PApp "S" [ PVar z]]
data Pat
  = PVar Name        -- zmienna, zawsze pasuje
  | PApp Name [Pat]  -- konstruktor i jego argumenty
  deriving (Show)


type DefMap = Map.Map Name Def


-- Parsowanie: z AST haskell-src do naszej skladni


fromHsString :: String -> [Match]
fromHsString src = validateMatches (fromParseResult (parseModule src))

fromParseResult :: ParseResult HsModule -> [Match]
fromParseResult (ParseOk hsModule)    = fromHsModule hsModule
fromParseResult (ParseFailed loc err) =
  error $ "Blad parsowania: " ++ err ++ " w miejscu " ++ show loc

fromHsModule :: HsModule -> [Match]
fromHsModule (HsModule _ _ _ _ decls) = concatMap transDecl decls

transDecl :: HsDecl -> [Match]
transDecl (HsFunBind matches)               = map transMatch matches
transDecl (HsPatBind _ (HsPVar name) rhs _) = [Match (transName name) [] (transRhs rhs)]
transDecl _                                 = []

transMatch :: HsMatch -> Match
transMatch (HsMatch _ name pats rhs _) =
  Match (transName name) (map transPat pats) (transRhs rhs)

transName :: HsName -> Name
transName (HsIdent  n) = n
transName (HsSymbol n) = n

transPat :: HsPat -> Pat
transPat (HsPVar name)               = PVar (transName name)
transPat (HsPApp (UnQual name) pats) = PApp (transName name) (map transPat pats)
transPat (HsPParen p)                = transPat p
transPat _                           = error "Nieobslugiwany wzorzec!"

transRhs :: HsRhs -> Expr
transRhs (HsUnGuardedRhs expr) = transExp expr
transRhs _                     = error "Nie przyjmujemy definicji z guardsami"

transExp :: HsExp -> Expr
transExp (HsApp l r)                   = transExp l :$ transExp r
transExp (HsVar (UnQual (HsIdent  n))) = Var n
transExp (HsVar (UnQual (HsSymbol n))) = Var n
transExp (HsCon (UnQual (HsIdent  n))) = Con n
transExp (HsCon (UnQual (HsSymbol n))) = Con n
transExp (HsParen e)                   = transExp e
transExp _                             = error "Skladnia nieobslugiwana ."


-- Budowanie mapy definicji


-- Grupuje klauzule po nazwie zachowujac kolejnosc
buildDefMap :: [Match] -> DefMap
buildDefMap = foldl step Map.empty
  where
    step acc m =
      Map.insertWith
        (\(Def newMs) (Def oldMs) -> Def (oldMs ++ newMs))
        (matchName m)
        (Def [m])
        acc


-- Walidacja


validateMatches :: [Match] -> [Match]
validateMatches = checkMain . checkUniqueArgs

checkMain :: [Match] -> [Match]
checkMain matches =
  let mainMs = filter (\m -> matchName m == "main") matches
  in case mainMs of
    []                                -> error "Blad Poprawnosci - brak funkcji main"
    (m : _) | not (null (matchPats m)) -> error "Blad Poprawnosci - funkcja main nie moze miec argumentow"
    _                                 -> matches

checkUniqueArgs :: [Match] -> [Match]
checkUniqueArgs matches =
  let badMatches = filter
        (\m -> let vs = patVarNames (matchPats m)
               in length (nub vs) /= length vs)
        matches
  in case badMatches of
    [] -> matches
    _  -> error "Blad Poprawnosci - duplikacja zmiennych we wzorcach"

patVarNames :: [Pat] -> [Name]
patVarNames = concatMap go
  where
    go (PVar x)    = [x]
    go (PApp _ ps) = concatMap go ps


-- Operacje na wyrazeniach

flattenApp :: Expr -> (Expr, [Expr])
flattenApp = go []
  where
    go acc (a :$ b) = go (b : acc) a
    go acc e        = (e, acc)

subst :: (Name, Expr) -> Expr -> Expr
subst (target, new) (Var n)
  | n == target = new
  | otherwise   = Var n
subst _           (Con n)  = Con n
subst s           (l :$ r) = subst s l :$ subst s r

alphaRenameMatch :: [Pat] -> Expr -> ([Pat], Expr)
alphaRenameMatch pats body =
  let vars    = patVarNames pats
      mapping = [(v, '#' : v) | v <- vars]
      newPats = map (alphaRenamePat mapping) pats
      newBody = foldl (\b (old, new) -> subst (old, Var new) b) body mapping
  in (newPats, newBody)

alphaRenamePat :: [(Name, Name)] -> Pat -> Pat
alphaRenamePat mapping (PVar x) = case lookup x mapping of
  Just x' -> PVar x'
  Nothing -> PVar x
alphaRenamePat mapping (PApp c ps) =
  PApp c (map (alphaRenamePat mapping) ps)


-- Wypisywanie wyrazen


-- Nawiasy stawiamy tylko gdy flaga jest True (prawy argument aplikacji).
paren :: Bool -> String -> String
paren True  s = "(" ++ s ++ ")"
paren False s = s

prettyGo :: Bool -> Expr -> String
prettyGo _ (Var n)  = n
prettyGo _ (Con n)  = n
prettyGo p (a :$ b) = paren p (prettyGo False a ++ " " ++ prettyGo True b)

prettyExpr :: Expr -> String
prettyExpr = prettyGo False


-- Poziom 2: redukcja krok po kroku z zaznaczonym kontekstem
--
-- Stan to cale wyrazenie plus sciezka do aktualnie obserwowanego
-- podwyrazenia (ogniska). Kazdy odwiedzony stan trafia do historii i jest
-- wypisywany z ogniskiem w klamrach {...}.


-- Sciezka od korzenia do ogniska: lewo (do funkcji) / prawo (do argumentu).
data Dir = DirL | DirR
  deriving (Show, Eq)

type Path = [Dir]

-- Wstawia nowe podwyrazenie pod wskazana sciezka.
plugAt :: Path -> Expr -> Expr -> Expr
plugAt []           _        new = new
plugAt (DirL : pth) (a :$ b) new = plugAt pth a new :$ b
plugAt (DirR : pth) (a :$ b) new = a :$ plugAt pth b new
plugAt _            e        _   = e

-- Wyciaga podwyrazenie spod wskazanej sciezki .
focusAt :: Path -> Expr -> Expr
focusAt []           e        = e
focusAt (DirL : pth) (a :$ _) = focusAt pth a
focusAt (DirR : pth) (_ :$ b) = focusAt pth b
focusAt _            e        = e

-- Wypisuje cale wyrazenie z ogniskiem w klamrach.
prettyFocused :: Path -> Expr -> String
prettyFocused path0 expr0 = go False path0 expr0
  where
    go _ []           e        = "{" ++ prettyExpr e ++ "}"
    go p (DirL : pth) (a :$ b) = paren p (go False pth a ++ " " ++ prettyGo True b)
    go p (DirR : pth) (a :$ b) = paren p (prettyGo False a ++ " " ++ go True pth b)
    go _ _            e        = "{" ++ prettyExpr e ++ "}"

type Step = (Path, Expr)

data EvalState = EvalState
  { esDefs  :: DefMap
  , esExpr  :: Expr
  , esFuel  :: Int
  , esTrace :: SnocList Step
  }

type Eval = State EvalState

hasFuel :: Eval Bool
hasFuel = gets ((> 0) . esFuel)

-- Podmienia podwyrazenie pod sciezka, zapisuje wynik jako nowy korzen i
-- dopisuje go do historii.
recordAt :: Path -> Expr -> Eval ()
recordAt path new = do
  ok <- hasFuel
  when ok $ modify $ \s ->
    let whole' = plugAt path (esExpr s) new
    in s { esExpr  = whole'
         , esTrace = snoc (esTrace s) (path, whole')
         , esFuel  = esFuel s - 1
         }

argPaths :: Int -> [Path]
argPaths n = [replicate (n - i) DirL ++ [DirR] | i <- [1 .. n]]

isReducibleHead :: DefMap -> Expr -> Bool
isReducibleHead defMap e =
  let (hd, args) = flattenApp e
  in case hd of
    Var name -> case Map.lookup name defMap of
      Just (Def ms) -> any (\m -> length args >= length (matchPats m)) ms
      Nothing       -> False
    _ -> False

focusExpr :: Path -> Eval Expr
focusExpr path = gets (focusAt path . esExpr)

note :: Path -> Eval ()
note path = do
  ok <- hasFuel
  when ok $ modify $ \s ->
    s { esTrace = snoc (esTrace s) (path, esExpr s)
      , esFuel  = esFuel s - 1
      }


-- Poziom 2: dopasowanie wzorcow i wymuszanie do WHNF


-- Redukuje ognisko do WHNF, powtarzajac redukcje glowy.
forceWhnf :: Path -> Eval ()
forceWhnf focus = do
  reduced <- stepHeadM focus
  when reduced (forceWhnf focus)

-- Jedna redukcja na pozycji glowy ogniska. Zwraca True gdy cos zredukowalismy .
stepHeadM :: Path -> Eval Bool
stepHeadM focus = do
  ok <- hasFuel
  if not ok
    then return False
    else do
      defMap <- gets esDefs
      e <- focusExpr focus
      let (hd, args) = flattenApp e
      case hd of
        Var name -> case Map.lookup name defMap of
          Just (Def ms) -> tryClausesM focus ms (length args)
          Nothing       -> return False
        _ -> return False

-- Probuje klauzule po kolei; pierwsza pasujaca jest redukowana.
tryClausesM :: Path -> [Match] -> Int -> Eval Bool
tryClausesM _     []       _ = return False
tryClausesM focus (m : ms) n
  | n < arity = tryClausesM focus ms n
  | otherwise = do
      let (renamedPats, renamedBody) = alphaRenameMatch (matchPats m) (matchRhs m)
          relPaths                   = take arity (argPaths n)
      mb <- matchPatsM focus (zip renamedPats relPaths)
      case mb of
        Just bindings -> do
          let result = foldl (flip subst) renamedBody bindings
          cur <- focusExpr focus
          let restArgs = drop arity (snd (flattenApp cur))
              newExpr  = foldl (:$) result restArgs
          recordAt focus newExpr
          return True
        Nothing -> tryClausesM focus ms n
  where
    arity = length (matchPats m)

-- Dopasowanie listy wzorcow lewo->prawo ale przy pierwszym
-- niepowodzeniu nie dotykamy kolejnych argumentow.
matchPatsM :: Path -> [(Pat, Path)] -> Eval (Maybe [(Name, Expr)])
matchPatsM _     []                = return (Just [])
matchPatsM focus ((p, rel) : rest) = do
  mb <- tryMatchPatM focus rel p
  case mb of
    Nothing -> return Nothing
    Just b1 -> do
      mb2 <- matchPatsM focus rest
      case mb2 of
        Nothing -> return Nothing
        Just b2 -> return (Just (b1 ++ b2))

-- Dopasowuje jeden wzorzec do argumentu pod focus++rel.
tryMatchPatM :: Path -> Path -> Pat -> Eval (Maybe [(Name, Expr)])
tryMatchPatM focus rel (PVar x) = do
  e <- focusExpr (focus ++ rel)
  return (Just [(x, e)])
tryMatchPatM focus rel (PApp con ps) = do
  defMap <- gets esDefs
  let absPath = focus ++ rel
  arg0 <- focusExpr absPath
  when (isReducibleHead defMap arg0) $ do
    note absPath
    forceWhnf absPath
  arg <- focusExpr absPath
  let (hd, sub) = flattenApp arg
  case hd of
    Con c | c == con && length sub == length ps ->
      matchPatsM focus (zip ps (map (rel ++) (argPaths (length sub))))
    _ -> return Nothing


-- Poziom 2: redukcja do postaci normalnej


-- Redukuje ognisko do postaci normalnej w kolejnosci najbardziej zewnetrznej
-- z lewej: najpierw glowa, a gdy juz nieredukowalna to kolejno argumenty.
eval :: Path -> Eval ()
eval focus = do
  ok <- hasFuel
  when ok $ do
    e <- focusExpr focus
    let (_, args) = flattenApp e
    reduced <- stepHeadM focus
    if reduced
      then eval focus
      else descendArgs focus (length args)

-- Schodzi ogniskiem w kolejne argumenty i normalizuje kazdy z nich.
descendArgs :: Path -> Int -> Eval ()
descendArgs focus n = mapM_ go (argPaths n)
  where
    go rel = do
      note (focus ++ rel)
      eval (focus ++ rel)

maxSteps :: Int
maxSteps = 100

runEval :: DefMap -> [String]
runEval defMap =
  let start     = Var "main"
      st0       = EvalState
        { esDefs  = defMap
        , esExpr  = start
        , esFuel  = maxSteps
        , esTrace = fromList [([], start)]
        }
      final     = execState (eval []) st0
      rows      = map (uncurry prettyFocused) (toList (esTrace final))
      truncated = esFuel final <= 0
  in rows ++ ["..." | truncated]


-- Punkt wejscia


printUsage :: IO ()
printUsage = putStrLn
  "Usage: zadanie3 [--help] [file]\n\
  \  --help  - display this message\n\
  \  file    - file with program to reduce"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    [plik]     -> do
      src <- readFile plik
      let matches = fromHsString src
          defMap  = buildDefMap matches
      mapM_ putStrLn (runEval defMap)
    _ -> printUsage