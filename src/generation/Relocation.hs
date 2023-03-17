module Relocation (RelocationTable, offsetRelocations, joinRelocationTables, RelocatableWriter, Relocatable (..), addRelocatable, executeRelocatableWriter, newRelocatableUnit, addLabel, resolveLabel) where

import Control.Exception (assert)
import Control.Monad.State (State, execState, gets, modify)
import Data.Binary (Put, Word64, Word8, putWord8)
import Data.List (sortOn)
import Data.Map (Map, empty, insert, lookup, mapAccum, notMember, singleton, union)
import Data.Ord (Down (Down))

-- Relocation table
type RelocationTable = Map String Word64

offsetRelocations :: Word64 -> RelocationTable -> RelocationTable
offsetRelocations a = fmap (+ a)

joinRelocationTables :: RelocationTable -> RelocationTable -> RelocationTable
joinRelocationTables = union

resolveLabel :: RelocationTable -> String -> Word64
resolveLabel rt l = case Data.Map.lookup l rt of
  Just addr -> addr
  Nothing -> error ("Label '" ++ l ++ "'" ++ " not found")

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
joinRelocatableAligned na (Relocatable ad as aa) (Relocatable bd bs ba) = (crel, off)
  where
    bfa = lcm na ba
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

addRelocatableToUnit :: RelocatableUnit -> Relocatable -> RelocatableUnit
addRelocatableToUnit (RelocatableUnit rela rt) relb = RelocatableUnit (fst $ joinRelocatable rela relb) rt

joinAlignedUnits :: Word64 -> RelocatableUnit -> RelocatableUnit -> RelocatableUnit
joinAlignedUnits alignment (RelocatableUnit ar art) (RelocatableUnit br brt) = RelocatableUnit cr crt
  where
    as = reloctableSize ar
    (cr, off) = joinRelocatableAligned alignment ar br
    boff = as + off
    crt = joinRelocationTables art (offsetRelocations boff brt)

joinUnits :: RelocatableUnit -> RelocatableUnit -> RelocatableUnit
joinUnits = joinAlignedUnits 1

joinUnitList :: [RelocatableUnit] -> RelocatableUnit
joinUnitList us = foldr joinUnits emptyRelocatableUnit sus
  where
    sus = sortOn (Down . (\(RelocatableUnit (Relocatable _ s _) _) -> s)) us

relocatableUnitSize :: RelocatableUnit -> Word64
relocatableUnitSize (RelocatableUnit rel _) = reloctableSize rel

addLabelToRelocatableUnit :: String -> RelocatableUnit -> RelocatableUnit
addLabelToRelocatableUnit l u = RelocatableUnit rel newRt
  where
    RelocatableUnit rel rt = u
    addr = reloctableSize rel
    newRt = assert (notMember l rt) $ insert l addr rt

-- Segment
newtype Segment = Segment [RelocatableUnit]

addLabelToSegment :: String -> Segment -> Segment
addLabelToSegment l (Segment []) = Segment [RelocatableUnit emptyRelocatable (singleton l 0)]
addLabelToSegment l (Segment (u : us)) = Segment (addLabelToRelocatableUnit l u : us)

addRelocatableToSegment :: Relocatable -> Segment -> Segment
addRelocatableToSegment rel (Segment []) = Segment [RelocatableUnit rel empty]
addRelocatableToSegment rel (Segment (u : us)) = Segment (addRelocatableToUnit u rel : us)

addUnitToSegment :: Segment -> Segment
addUnitToSegment (Segment us) = Segment (emptyRelocatableUnit : us)

newtype RelocatableWriterState = State {segments :: Map String Segment}

setSegment :: String -> Segment -> RelocatableWriterState -> RelocatableWriterState
setSegment sname segment state = state {segments = insert sname segment ss}
  where
    ss = segments state

initialRelocatableWriterState :: RelocatableWriterState
initialRelocatableWriterState = State empty

joinSegmentUnits :: Segment -> RelocatableUnit
joinSegmentUnits (Segment us) = joinUnitList us

type RelocatableWriter = State RelocatableWriterState

modifySegment :: String -> (Segment -> Segment) -> RelocatableWriter ()
modifySegment sname fn = do
  segmentsMap <- gets segments

  segment <- case Data.Map.lookup sname segmentsMap of
    Just segment -> do
      return (fn segment)
    Nothing -> do
      return (fn (Segment []))

  modify (setSegment sname segment)
  return ()

addLabel :: String -> String -> RelocatableWriter ()
addLabel sname l = do
  modifySegment sname $ addLabelToSegment l
  return ()

addRelocatable :: String -> Relocatable -> RelocatableWriter ()
addRelocatable sname rel = do
  modifySegment sname $ addRelocatableToSegment rel
  return ()

newRelocatableUnit :: String -> RelocatableWriter ()
newRelocatableUnit sname = do
  modifySegment sname addUnitToSegment
  return ()

type SegmentTable = Map String Word64

assembleNamedUnits :: Word64 -> Map String RelocatableUnit -> (RelocatableUnit, SegmentTable)
assembleNamedUnits alignment =
  mapAccum
    ( \acc u ->
        ( joinAlignedUnits alignment acc u,
          relocatableUnitSize acc
        )
    )
    emptyRelocatableUnit

executeRelocatableWriter :: RelocatableWriter () -> (RelocationTable -> Put, RelocationTable, SegmentTable)
executeRelocatableWriter fn = (putRelocatable rel, rt, st)
  where
    (State segmentsMap) = execState fn initialRelocatableWriterState
    namedUnits = fmap joinSegmentUnits segmentsMap
    (RelocatableUnit rel rt, st) = assembleNamedUnits 4096 namedUnits
