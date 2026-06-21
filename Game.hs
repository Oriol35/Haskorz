module Game (Mode (..), juga, estatInicial, soluciona) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
-- Used for State monads
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans.Class (lift)
import Controller (Direccio (..), viToDireccio)
import Data.List (delete, group, unfoldr)
import Parser (Instance (..), Posicio, Tauler)
import Utils (maybeHead)

-- Only 3 ways to be oriented. (N, ←→, ↑)
data OrientacioNucli = Dret | Horitzontal | Vertical deriving (Eq, Ord, Show)

data Nucli = Nucli
  { base :: Posicio,
    orientacio :: OrientacioNucli
  }
  deriving (Eq, Ord, Show)

-------------- STATE LOGIC --------------

-- JocConfig is immutable.
-- since JocEstat is responsible for mutable state.
data JocConfig = JocConfig
  { board :: Tauler,
    goal :: Posicio,
    start :: Posicio,
    coreH :: Int,
    nRows :: Int,
    nCols :: Int
  }

-- JocEstat is the mutable game state.
-- (player position and the number of moves)
-- Better since we change only what we need.
data JocEstat = JocEstat
  { player :: Nucli,
    moves :: Int
  }
--  deriving (Eq)

-- Game modes
data Mode = AI | Huma deriving (Show)

-- JocMonad is the game monad stack:
--   * StateT JocEstat      mutable state of the game
--   * ReaderT JocConfig    immutable game config
--   * IO                   console input/output
-- Using ReaderT avoids copying board/goal/start on every state update.
type JocMonad = StateT JocEstat (ReaderT JocConfig IO)

-- Initial game (Instance to parts of JocMonad)
estatInicial :: Instance -> (JocConfig, JocEstat)
estatInicial (Instance coreHeight nRows nCols boardLines startPos goalPos) =
  ( JocConfig
      { board = boardLines,
        goal = goalPos,
        start = startPos,
        coreH = coreHeight,
        nRows = nRows,
        nCols = nCols
      },
    JocEstat
      { player = Nucli startPos Dret,
        moves = 0
      }
  )

-------------- GAME LOOP --------------

-- (!) compactMoviments makes solutions more readable.
juga :: Mode -> JocConfig -> JocEstat -> IO ()
juga AI config estat =
  case soluciona config of
    Nothing -> putStrLn "Colapse energètic a l'estació Monad."
    Just passos -> do
      putStrLn $ showJoc config estat
      putStrLn $ "Solució trobada en " ++ show (length passos) ++ " passos."
      putStrLn $ "Seqüència de passos: " ++ compactaMoviments passos

-- Manual mode
juga Huma config estat = runReaderT (evalStateT loop estat) config
  where
    -- evalStateT runs the state monad
    -- runReaderT runs the monad + config so only 1 thing chaenges.
    loop :: JocMonad ()
    loop = do
      -- 'get' returns current state monad (JocEstat)
      -- so we can get current mutable state
      estatActual <- get

      -- Read immutable data.
      -- 'lift' is needed because ReaderT is in our way
      -- 'ask' reads the config (JocConfig).
      cfg <- lift ask
      -- I used 'unless' thanks to the LSP, looks more readable
      unless (connectatAlReactor estatActual cfg) $ do
        liftIO $ putStrLn $ showJoc cfg estatActual
        input <- liftIO $ maybeHead <$> getLine
        case input >>= viToDireccio of
          Nothing -> do
            liftIO $ putStrLn "Invalid input, try again."
            loop
          Just direc -> do
            -- Update StateT
            maniobra direc
            -- Read the new state
            next <- get
            if succionat cfg (player next)
              then do
                liftIO $ putStrLn "El nucli ha caigut. Torna a la posicio inicial."
                modify (\st -> st {player = Nucli (start cfg) Dret})
                loop
              else
                if connectatAlReactor next cfg
                  then do
                    liftIO $ putStrLn $ showJoc cfg next
                    liftIO $ putStrLn "Sistema energètic estabilitzat!"
                  else loop

-- Shows static + dinamic game state on screen. See 'dibuixaTauler'
showJoc :: JocConfig -> JocEstat -> String
showJoc cfg est =
  unlines
    [ "Moves: " ++ show (moves est),
      "Goal: " ++ show (goal cfg),
      dibuixaTauler cfg (player est)
    ]

-- Merges config + 'Nucli' to a single String
-- 'zipWith' helps us do "dibuixaFila N Nrow"
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:zipWith
dibuixaTauler :: JocConfig -> Nucli -> String
dibuixaTauler cfg nucli =
  unlines $ zipWith dibuixaFila [0 ..] (board cfg)
  where
    -- current position
    occupied = localitzaNucli cfg nucli
    dibuixaFila f row = map (dibuixaCela f) (zip [0 ..] row)
    -- changes (or not) occupied positions 
    -- (position, char)
    dibuixaCela f (c, char)
      | (f, c) `elem` occupied = 'N'
      | otherwise = char

estaDret :: Nucli -> Bool
estaDret (Nucli _ Dret) = True
estaDret _ = False

-- Check if a position is inside the board
dinsTauler :: Posicio -> JocConfig -> Bool
dinsTauler (f, c) cfg =
  f >= 0 && c >= 0 && f < nRows cfg && c < nCols cfg

-- Idk if that's ineficient
-- But "!!" might be our best tool. Couldn't find a better way.
posicioCella :: Posicio -> Tauler -> Char
posicioCella (f, c) tauler = (tauler !! f) !! c

-- Movment abstraction to avoid repeating ourselves.
delta :: Direccio -> (Int, Int)
delta Oest = (0, -1)
delta Est = (0, 1)
delta Nord = (-1, 0)
delta Sud = (1, 0)

mouPosicio :: Posicio -> (Int, Int) -> Posicio
mouPosicio (f, c) (df, dc) = (f + df, c + dc)

desplacar :: Posicio -> Direccio -> Posicio
desplacar pos direccio = mouPosicio pos (delta direccio)

-- Check whether a board cell is "gel" (~)
esGel :: Posicio -> JocConfig -> Bool
esGel pos cfg = dinsTauler pos cfg && posicioCella pos (board cfg) == '~'

-- The core is completely on gel if all occupied positions are '~'
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:all
-- ↑ Exactly like (and (map ...)). Another LSP recommendation.
dinsGel :: JocConfig -> Nucli -> Bool
dinsGel cfg nucli =
  all f ocupades
  where
    ocupades = localitzaNucli cfg nucli
    f pos = esGel pos cfg

-- Shift the core in the same orientation
-- (!) does not count as movement
llisca :: Direccio -> Nucli -> Nucli
llisca direccio nucli = nucli {base = desplacar (base nucli) direccio}

-- Repeaded movement when we're sliding on ice.
finalitzaLliscament :: JocConfig -> Direccio -> Nucli -> Nucli
finalitzaLliscament cfg direccio nucli =
  let caminar actual =
        let properNext = llisca direccio actual
         in if dinsGel cfg properNext
              then caminar properNext
              else (actual, properNext)
      (ultimSobreGel, proper) = caminar nucli
   in -- when "Dret" we'll fall when exiting the ice
      if estaDret ultimSobreGel && not (dinsGel cfg proper)
        then calculaManiobra cfg direccio ultimSobreGel
        -- otherwise we slide out.
        else llisca direccio ultimSobreGel

-- Apply the according movement.
-- finalitzaLliscament happens whenever a move ends on ice.
accioFinal :: JocConfig -> Direccio -> Nucli -> Nucli
accioFinal cfg direccio nucli =
  let normal = calculaManiobra cfg direccio nucli
   in if not (esSegur cfg normal) || not (dinsGel cfg normal)
        then normal
        else finalitzaLliscament cfg direccio normal

-- calculates the next move according to core orientation
-- then we move the position and orientation accordingly.
-- this could be made better but I'm unsure.
calculaManiobra :: JocConfig -> Direccio -> Nucli -> Nucli
calculaManiobra cfg direccio (Nucli p1 orient) =
  let n = coreH cfg
   in case orient of
        Dret -> maniobraDret direccio p1 n
        Horitzontal -> maniobraHoritzontal direccio p1 n
        Vertical -> maniobraVertical direccio p1 n

-- "n" stands for "core height", ok?
maniobraDret :: Direccio -> Posicio -> Int -> Nucli
maniobraDret direccio (f, c) n =
  case direccio of
    Nord -> Nucli (f - n, c) Vertical
    Sud -> Nucli (f + 1, c) Vertical
    Oest -> Nucli (f, c - n) Horitzontal
    Est -> Nucli (f, c + 1) Horitzontal

maniobraHoritzontal :: Direccio -> Posicio -> Int -> Nucli
maniobraHoritzontal direccio (f, c) n =
  case direccio of
    Oest -> Nucli (f, c - 1) Dret
    Est -> Nucli (f, c + n) Dret
    Nord -> Nucli (f - 1, c) Horitzontal
    Sud -> Nucli (f + 1, c) Horitzontal

maniobraVertical :: Direccio -> Posicio -> Int -> Nucli
maniobraVertical direccio (f, c) n =
  case direccio of
    Nord -> Nucli (f - 1, c) Dret
    Sud -> Nucli (f + n, c) Dret
    Oest -> Nucli (f, c - 1) Vertical
    Est -> Nucli (f, c + 1) Vertical

-- See 'maniobra' further below
-- and what 'accioFinal' does.
aplicaManiobra :: JocConfig -> Direccio -> JocEstat -> JocEstat
aplicaManiobra cfg direccio estat =
  let nextNucli = accioFinal cfg direccio (player estat)
   in estat {player = nextNucli, moves = moves estat + 1}

--- AI MODE ---

-- A* will take into account:
-- costTotal (f): g + h
--  costReal (g): steps taken so far
--  manhattan (h): sum of dimensional distance to goal (https://en.wikipedia.org/wiki/Taxicab_geometry)

-- Helper types
type Cost = Int

-- (!) Data structures ahead

-- Stores a node and the previous "Nucli + step".
-- branch child -> (direction, parent) 
type Origen = [(Nucli, (Direccio, Nucli))]

-- List of visited states.
-- lowest cost (g) for each state.
type CostosActuals = [(Nucli, Cost)]

-- Obert: Pending states (to be explored).
-- I'm just copying nomenclatures learnt at AI class.
type Oberts = [NodeObert]

-- NodeObert: Nucli + costs.
-- costTotal = g + h
-- costReal(steps taken) = g
data NodeObert = NodeObert
  { costTotal :: Int,
    costReal :: Cost,
    estatActual :: Nucli
  }
  deriving (Eq, Show)

-- Order preference when comparing 2 nodes
instance Ord NodeObert where
  compare a b
    | costTotal a /= costTotal b = compare (costTotal a) (costTotal b)
    | costReal a /= costReal b = compare (costReal a) (costReal b)
    | otherwise = compare (estatActual a) (estatActual b)

-- Manhattan distance (deltaX + deltaY).
manhattan :: Posicio -> Posicio -> Int
manhattan (f1, c1) (f2, c2) = abs (f1 - f2) + abs (c1 - c2)

-- A* guarantees an optimal solution (manhattan >= real cost)
-- by -> minimizing <- heuristic:
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:minimum
heuristica :: JocConfig -> Nucli -> Posicio -> Int
heuristica cfg nucli goalPos = minimum (map (\pos -> manhattan pos goalPos) (localitzaNucli cfg nucli))

-- Possible SAFE moves from Nucli.
-- (!) now 'accioFinal' handles ice movments.
veinat :: JocConfig -> Nucli -> [(Direccio, Nucli)]
veinat cfg nucli =
  [ (dir, neighbor) | dir <- [Oest, Sud, Nord, Est],
                      let neighbor = accioFinal cfg dir nucli,
                      esSegur cfg neighbor
  ]

-- A* across Nucli's possible moves.
-- We always start with initial core position and 'Dret'.
aEstrella :: JocConfig -> Maybe [Direccio]
aEstrella cfg = buscar obertsInici origenInici gInicial
  where
    startNucli = Nucli (start cfg) Dret
    obertsInici = [NodeObert (heuristica cfg startNucli (goal cfg)) 0 startNucli]
    origenInici = []
    gInicial = [(startNucli, 0)]

    buscar :: Oberts -> Origen -> CostosActuals -> Maybe [Direccio]
    buscar [] _ _ = Nothing
    buscar oberts origen costosActuals =
      -- Best node (lowest f, g, or state).
      -- We pick it up
      let nodeActual = minimum oberts
          obertsRestants = delete nodeActual oberts
          estatActualNode = estatActual nodeActual
          -- Best known cost (g) for this state
          -- I know the case is unnecessary but it can be "Nothing" so..
          gActual =
            case lookup estatActualNode costosActuals of
              Nothing -> error "Missing cost"
              Just cost -> cost
      in  if costReal nodeActual > gActual
            then buscar obertsRestants origen costosActuals
          else
              -- Goal reached! Generate the answer.
              if connectatAlReactor (JocEstat estatActualNode 0) cfg
                then Just (generaIndicacions estatActualNode origen)
              else
                -- Search for neighbours.
                let (oberts', origen', costosActuals') =
                      foldl
                        (exploraVeinat estatActualNode gActual)
                        (obertsRestants, origen, costosActuals)
                        (veinat cfg estatActualNode)
                  in buscar oberts' origen' costosActuals'

    -- Explore one neighboring state.
    -- If the new path is better, update its cost and origin.
    exploraVeinat :: Nucli -> Cost -> (Oberts, Origen, CostosActuals) -> (Direccio, Nucli) -> (Oberts, Origen, CostosActuals)
    exploraVeinat actual gActual (obertsAcc, origenAcc, costosActualsAcc) (dir, vei) =
      let costTeoric = gActual + 1
          costAntic = lookup vei costosActualsAcc
          actualitza =
            let h = heuristica cfg vei (goal cfg)
                nodeNou = NodeObert (costTeoric + h) costTeoric vei
                -- Add the node to our collection
                -- and get its origin.
                obertsActualitzats = nodeNou : obertsAcc
                origenActualitzat = (vei, (dir, actual)) : origenAcc
                costosActualsActualitzats = (vei, costTeoric) : costosActualsAcc
            in (obertsActualitzats, origenActualitzat, costosActualsActualitzats)

      in case costAntic of
            Just costVell ->
              if costTeoric >= costVell
                then (obertsAcc, origenAcc, costosActualsAcc)
                else actualitza

            Nothing ->
              actualitza

    -- Note: Before unfoldr I was making custom functions...
      -- But then found out there's a more stylish way to generate the path
      -- So we can follow the parent liks backwards and generate indications
      -- (https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:unfoldr).
    generaIndicacions :: Nucli -> Origen -> [Direccio]
    generaIndicacions actual origenMap =
      reverse $ unfoldr (\node -> lookup node origenMap) actual

-- Display movement sequence in order to be displayed in a more readable way.
-- Better than seing simulated steps (IMO).
-- I also wanted to do this because looks likea great idea.
-- Very USEFUL functions that help it easy:
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-String.html#v:unwords
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:group
compactaMoviments :: [Direccio] -> String
compactaMoviments = unwords . map compactar . group
  where
    compactar direccions = show (head direccions) ++ "(x" ++ show (length direccions) ++ ")"

-------------- OBLIGATORY FUNCTIONS --------------

-- Returns all positions occupied for a given "Nucli"
-- Takes into account core height
-- Very useful with list comprenhensions!
localitzaNucli :: JocConfig -> Nucli -> [Posicio]
localitzaNucli cfg (Nucli (f, c) orientacio)
  | orientacio == Dret = [(f, c)]
  | orientacio == Horitzontal = [(f, c + offset) | offset <- [0 .. n - 1]]
  | otherwise = [(f + offset, c) | offset <- [0 .. n - 1]]
  where
    -- As always "n" i core height
    n = coreH cfg

-- END LOGIC: we're on top of the goal standing upright
connectatAlReactor :: JocEstat -> JocConfig -> Bool
connectatAlReactor estat cfg =
  estaDret nucli && localitzaNucli cfg nucli == [goal cfg]
  where
    nucli = player estat

-- Check a board cell is a platform inside Tauler
esPlataforma :: Posicio -> JocConfig -> Bool
esPlataforma pos cfg = dinsTauler pos cfg && posicioCella pos (board cfg) /= '0'

-- We are safe if all occupied positions are platforms.
esSegur :: JocConfig -> Nucli -> Bool
esSegur cfg nucli = all (\pos -> esPlataforma pos cfg) (localitzaNucli cfg nucli)

-- It took a while to notice why we need to pass "()"
-- We just want to modify using 'aplicaManiobra'.
maniobra :: Direccio -> JocMonad ()
maniobra direccio = do
  cfg <- lift ask 
  modify (aplicaManiobra cfg direccio)

-- If the player is not safe, it is sucked into the void
succionat :: JocConfig -> Nucli -> Bool
succionat cfg nucli = not (esSegur cfg nucli)

-- Execute A* algorithm using our input Instance
-- We only need JocConfig (static)
soluciona :: JocConfig -> Maybe [Direccio]
soluciona = aEstrella
