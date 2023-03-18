module Relocation (RelocationTable, offsetRelocations, joinRelocationTables, RelocatableWriter, Relocatable (..), addRelocatable, getUniqueLabel, addLabeledRelocatable, executeRelocatableWriter, newRelocatableUnit, addLabel, addUniqueLabel, resolveLabel, getRelativeAddress, SegmentType (..), addSegment) where

import Control.Exception (assert)
import Control.Monad.State (State, execState, gets, modify)
import Data.Binary (Put, Word64, Word8, putWord8)
import Data.List (mapAccumL, sortOn)
import Data.Map (Map, empty, findWithDefault, fromList, insert, intersection, lookup, notMember, singleton, union)
import Data.Ord (Down (Down))

-- Relocation table
type RelocationTable = Map String Word64

offsetRelocations :: Word64 -> RelocationTable -> RelocationTable
offsetRelocations a = fmap (+ a)

joinRelocationTables :: RelocationTable -> RelocationTable -> RelocationTable
joinRelocationTables a b = if intersection a b == empty then a `union` b else error "Duplicate labels found..."

resolveLabel :: RelocationTable -> String -> Word64
resolveLabel rt l = case Data.Map.lookup l rt of
  Just addr -> addr
  Nothing -> error ("Label '" ++ l ++ "'" ++ " not found")

getRelativeAddress :: RelocationTable -> String -> String -> Word64
getRelativeAddress rt dest_label from_label = resolveLabel rt dest_label - resolveLabel rt from_label

-- Relocatable
data Relocatable = Relocatable {relocatableData :: RelocationTable -> Put, reloctableSize :: Word64, relocatableAlignment :: Word64}

emptyRelocatable :: Relocatable
emptyRelocatable = Relocatable (const $ pure ()) 0 1

putRelocatable :: Relocatable -> RelocationTable -> Put
putRelocatable (Relocatable d _ _) = d

pad :: Word8 -> Word64 -> Put
pad _ 0 = pure ()
pad v n = do
  pad v (n - 1)
  putWord8 v

joinRelocatableAligned :: Word64 -> Relocatable -> Relocatable -> (Relocatable, Word64)
joinRelocatableAligned alignment (Relocatable ad as aa) (Relocatable bd bs ba) = (crel, off)
  where
    bfa = lcm alignment ba
    off = if as `mod` bfa == 0 then 0 else bfa - (as `mod` bfa)
    cd rt = do
      ad rt
      pad 0 off
      bd rt
    cs = as + off + bs
    biga = max aa bfa
    smalla = min aa bfa
    ca = assert (biga `mod` smalla == 0) biga
    crel = Relocatable cd cs ca

joinRelocatable :: Relocatable -> Relocatable -> (Relocatable, Word64)
joinRelocatable = joinRelocatableAligned 1

-- RelocatableUnit
data RelocatableUnit = RelocatableUnit Relocatable RelocationTable

emptyRelocatableUnit :: RelocatableUnit
emptyRelocatableUnit = RelocatableUnit emptyRelocatable empty

fillerRelocatableUnit :: Word64 -> RelocatableUnit
fillerRelocatableUnit size = RelocatableUnit (Relocatable (const $ pure ()) size 1) empty

addLabelToRelocatableUnit :: String -> RelocatableUnit -> RelocatableUnit
addLabelToRelocatableUnit l u = RelocatableUnit rel newRt
  where
    RelocatableUnit rel rt = u
    addr = reloctableSize rel
    newRt = if notMember l rt then insert l addr rt else error ("Label '" ++ l ++ "'" ++ " already exists")

addRelocatableToUnit :: Maybe String -> RelocatableUnit -> Relocatable -> RelocatableUnit
addRelocatableToUnit ml (RelocatableUnit rela rt) relb = RelocatableUnit relc newRt
  where
    (relc, off) = joinRelocatable rela relb
    addr = reloctableSize rela + off
    newRt = case ml of
      Just l -> if notMember l rt then insert l addr rt else error ("Label '" ++ l ++ "'" ++ " already exists")
      Nothing -> rt

joinAlignedUnits :: Word64 -> RelocatableUnit -> RelocatableUnit -> (RelocatableUnit, Word64)
joinAlignedUnits alignment (RelocatableUnit ar art) (RelocatableUnit br brt) = (RelocatableUnit cr crt, boff)
  where
    as = reloctableSize ar
    (cr, off) = joinRelocatableAligned alignment ar br
    boff = as + off
    crt = joinRelocationTables art (offsetRelocations boff brt)

joinUnits :: RelocatableUnit -> RelocatableUnit -> RelocatableUnit
joinUnits a b = fst $ joinAlignedUnits 1 a b

joinUnitList :: [RelocatableUnit] -> RelocatableUnit
joinUnitList us = foldr joinUnits emptyRelocatableUnit sus
  where
    sus = sortOn (Down . (\(RelocatableUnit (Relocatable _ s _) _) -> s)) us

relocatableUnitSize :: RelocatableUnit -> Word64
relocatableUnitSize (RelocatableUnit rel _) = reloctableSize rel

-- Segment
data SegmentType = Exec | ROData | Data

data Segment = Segment [RelocatableUnit] Word64 SegmentType

createSegment :: Word64 -> Word64 -> SegmentType -> Segment
createSegment offset = Segment [fillerRelocatableUnit offset]

addLabelToSegment :: String -> Segment -> Segment
addLabelToSegment l (Segment [] a t) = Segment [RelocatableUnit emptyRelocatable (singleton l 0)] a t
addLabelToSegment l (Segment (u : us) a t) = Segment (addLabelToRelocatableUnit l u : us) a t

addRelocatableToSegment :: Maybe String -> Relocatable -> Segment -> Segment
addRelocatableToSegment ml rel (Segment [] a t) = Segment [addRelocatableToUnit ml emptyRelocatableUnit rel] a t
addRelocatableToSegment ml rel (Segment (u : us) a t) = Segment (addRelocatableToUnit ml u rel : us) a t

addUnitToSegment :: Segment -> Segment
addUnitToSegment (Segment us a t) = Segment (emptyRelocatableUnit : us) a t

data RelocatableWriterState = State {segments :: Map String Segment, segmentOrder :: [String], counter :: Word64}

incrementCounter :: RelocatableWriterState -> RelocatableWriterState
incrementCounter state = state {counter = nc}
  where
    nc = counter state + 1

addSegmentOrder :: String -> RelocatableWriterState -> RelocatableWriterState
addSegmentOrder sname state = state {segmentOrder = segmentOrder state ++ [sname]}

setSegment :: String -> Segment -> RelocatableWriterState -> RelocatableWriterState
setSegment sname segment state = state {segments = insert sname segment ss}
  where
    ss = segments state

initialRelocatableWriterState :: RelocatableWriterState
initialRelocatableWriterState = State empty [] 0

type RelocatableWriter = State RelocatableWriterState

addSegment :: String -> Word64 -> Word64 -> SegmentType -> RelocatableWriter ()
addSegment sname soff sal st = do
  segmentsMap <- gets segments
  case Data.Map.lookup sname segmentsMap of
    Just _ -> error ("Segment '" ++ sname ++ "' already existing.")
    Nothing -> do
      modify $ setSegment sname $ createSegment soff sal st
      modify $ addSegmentOrder sname
  return ()

modifySegment :: String -> (Segment -> Segment) -> RelocatableWriter ()
modifySegment sname fn = do
  segmentsMap <- gets segments

  segment <- case Data.Map.lookup sname segmentsMap of
    Just segment -> do
      return (fn segment)
    Nothing -> do
      error ("Segment '" ++ sname ++ "' not existing.")

  modify (setSegment sname segment)
  return ()

addLabel :: String -> String -> RelocatableWriter ()
addLabel sname l = do
  modifySegment sname $ addLabelToSegment l
  return ()

getUniqueLabel :: RelocatableWriter String
getUniqueLabel = do
  c <- gets counter
  let l = "@" ++ show c ++ "@"
  modify incrementCounter
  return l

addUniqueLabel :: String -> RelocatableWriter String
addUniqueLabel sname = do
  l <- getUniqueLabel
  addLabel sname l
  return l

addRelocatable :: String -> Relocatable -> RelocatableWriter ()
addRelocatable sname rel = do
  modifySegment sname $ addRelocatableToSegment Nothing rel
  return ()

addLabeledRelocatable :: String -> String -> Relocatable -> RelocatableWriter ()
addLabeledRelocatable sname l rel = do
  modifySegment sname $ addRelocatableToSegment (Just l) rel
  return ()

newRelocatableUnit :: String -> RelocatableWriter ()
newRelocatableUnit sname = do
  modifySegment sname addUnitToSegment
  return ()

type SegmentTable = Map String (Word64, Word64, SegmentType)

joinSegments :: Map String Segment -> RelocatableUnit -> String -> (RelocatableUnit, (String, (Word64, Word64, SegmentType)))
joinSegments segmentsMap acc sname = (ju, (sname, (ss, boff, st)))
  where
    Segment sus sal st = findWithDefault (error "Order list and map of segements out of sync") sname segmentsMap
    su = joinUnitList sus
    ss = relocatableUnitSize su
    (ju, boff) = joinAlignedUnits sal acc su

assembleSegments :: Map String Segment -> [String] -> (RelocatableUnit, [(String, (Word64, Word64, SegmentType))])
assembleSegments segmentsMap = mapAccumL (joinSegments segmentsMap) emptyRelocatableUnit

executeRelocatableWriter :: RelocatableWriter () -> (RelocationTable -> Put, RelocationTable, SegmentTable)
executeRelocatableWriter fn = (putRelocatable rel, rt, fromList st)
  where
    (State segmentsMap order _) = execState fn initialRelocatableWriterState
    (RelocatableUnit rel rt, st) = assembleSegments segmentsMap order
