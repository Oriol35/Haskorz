module Parser
  ( CoreHeight,
    NRows,
    NColumns,
    Tauler,
    Posicio,
    Instance (..),
    parseInstance,
  )
where

-- For some reason the order of the parameters was another in the file
-- Current format:
-- 2    (CoreHeight, >=1)
-- 6    (6 rows)
-- 10   (10 columns)
-- 1110000000
-- 1S11110000
-- 111~~~~110
-- 011~~~~111
-- 0000011G11
-- 0000001110

-- useful to searh the index of a given element
--- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html#v:elemIndex
import Data.List (elemIndex)
-- used this to read files safely
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Text-Read.html#v:readMaybe
import Text.Read (readMaybe)

type Posicio = (Int, Int)
type CoreHeight = Int
type NColumns = Int
type NRows = Int
type Tauler = [String]

-- old: data Instance = Instance CoreHeight NColumns NRows TableLines deriving Show
-- Now we store start / end positions 
data Instance = Instance CoreHeight NRows NColumns Tauler Posicio Posicio deriving (Show)

-- Find the 'Posicio' of a character in the board layout
findChar :: Char -> Tauler -> Maybe Posicio
findChar c rows = search 0 rows -- Start searching at row '0'
  where
    search _ [] = Nothing
    search r (x : xs) =
      case elemIndex c x of
        Just col -> Just (r, col)
        Nothing -> search (r + 1) xs

-- This turns .txt game file's content into an Instance.
-- I added every safeguard I cound think of.
parseInstance :: String -> Instance
parseInstance fileContent =
  -- lines: fastest way to -> [String]
  case lines fileContent of
    -- We expect:
    -- 1st line -> core height          (>=1)
    -- 2nd line -> number of rows       (>0)
    -- 3rd line -> number of columns    (>0)
    -- remaining lines -> board layout  (only 0/1/S/G/~, plus one S/G each)
    (height : rows : cols : boardLines) ->
      case (readMaybe height, readMaybe rows, readMaybe cols) of
        -- Type casting
        (Just coreH, Just nRows, Just nCols) ->
          -- Constraints
          if coreH < 1
            then error "PARSE_ERROR: Core height must be >= 1"
            else
              if nRows <= 0
                then error "PARSE_ERROR: Number of rows must be > 0"
                else
                  if nCols <= 0
                    then error "PARSE_ERROR: Number of columns must be > 0"
                    else
                      if length boardLines /= nRows
                        then error "PARSE_ERROR: Incorrect number of rows."
                        -- Validate row sizes
                        else
                          if not (all (\row -> length row == nCols) boardLines)
                            then error "PARSE_ERROR: Incorrect column size."
                            else
                              -- Strictly 1/0/S/G/~
                              let validChars = "01SG~"
                                  -- we joinin all rows
                                  allCells = concat boardLines
                                  -- and count the number of special cells
                                  sNum = length (filter (== 'S') allCells)
                                  gNum = length (filter (== 'G') allCells)
                               in -- Validate allowed characters
                                  if not (all (\c -> c `elem` validChars) allCells)
                                    then error "PARSE_ERROR: Invalid characters inside board. Use 0/1/S/G/~."
                                    -- ONLY 1 Start
                                    else
                                      if sNum /= 1
                                        then error "PARSE_ERROR: There must be exactly one S."
                                        -- ONLY 1 GOal
                                        else
                                          if gNum /= 1
                                            then error "PARSE_ERROR: There must be exactly one G."
                                            else case (findChar 'S' boardLines, findChar 'G' boardLines) of
                                              (Just startPos, Just goalPos) ->
                                                Instance coreH nRows nCols (cleanBoard boardLines) startPos goalPos
                                              _ ->
                                                error "PARSE_ERROR: Missing S or G"
        -- Not integers...
        _ -> error "PARSE_ERROR: Invalid header numbers"
        -- Not enough lines
    _ -> error "PARSE_ERROR: File too short"

-- We want to remove 'S'
-- once codified inside an Instance in form of 'Posicio'
cleanBoard :: Tauler -> Tauler
cleanBoard = map (map removeStart)
  where
    -- sweep across the board and only substitute 'S'
    removeStart 'S' = '1'
    removeStart c = c
