{-# LANGUAGE OverloadedStrings #-}

module Analyze
  ( extractFlatRules
  , runQuery1
  , runQuery2
  , runQuery3
  , runQuery4
  , formatQuery1
  , formatQuery2
  , formatQuery3
  , formatQuery4
  ) where

import Types
import PrologEmitter (flattenRule)
import Render (renderFlatFilterUnquoted)
import Data.Text (Text)
import qualified Data.Text as T

extractFlatRules :: SandboxProfile -> [FlatRule]
extractFlatRules profile = concatMap flattenRule (profileRules profile)

readOperations :: [Text]
readOperations =
  [ "file-readSTAR"
  , "file-read-data"
  , "file-read-metadata"
  ]

writeOperations :: [Text]
writeOperations =
  [ "file-writeSTAR"
  , "file-write-unlink"
  , "file-write-create"
  , "file-write-data"
  , "file-write-mode"
  ]

thirdPartyExtensions :: [Text]
thirdPartyExtensions =
  [ "com.apple.tcc.kTCCServiceAddressBook"
  , "com.apple.tcc.kTCCServicePhotos"
  , "com.apple.sandbox.container"
  ]

isContainerFilter :: FlatFilter -> Bool
isContainerFilter (FlatGeneric name [FlatStringVal "com.apple.sandbox.container"])
  | T.toLower name == "extension" = True
isContainerFilter (FlatGeneric name [FlatAtomVal "com.apple.sandbox.container"])
  | T.toLower name == "extension" = True
isContainerFilter _ = False

isCapability :: FlatFilter -> Bool
isCapability (FlatGeneric name _) | T.toLower name == "extension" = True
isCapability (FlatEntitlement _ _) = True
isCapability FlatDebugMode = True
isCapability _ = False

isThirdPartyCap :: FlatFilter -> Bool
isThirdPartyCap (FlatGeneric name [FlatStringVal ext])
  | T.toLower name == "extension" = ext `elem` thirdPartyExtensions
isThirdPartyCap (FlatGeneric name [FlatAtomVal ext])
  | T.toLower name == "extension" = ext `elem` thirdPartyExtensions
isThirdPartyCap _ = False

isFileFilter :: FlatFilter -> Bool
isFileFilter (FlatSubpath _) = True
isFileFilter (FlatLiteral _) = True
isFileFilter (FlatRegex _)   = True
isFileFilter _               = False

isVnode :: FlatFilter -> Bool
isVnode (FlatGeneric name _) | T.toLower name == "vnode-type" = True
isVnode _                    = False

overlap :: FlatFilter -> FlatFilter -> Bool
overlap f1 f2
  | f1 == f2 = True
  | otherwise = pathAccess f1 f2 || pathAccess f2 f1

pathAccess :: FlatFilter -> FlatFilter -> Bool
pathAccess (FlatLiteral target) (FlatSubpath parent) = parent `T.isPrefixOf` target
pathAccess (FlatSubpath target) (FlatSubpath parent) = parent `T.isPrefixOf` target
pathAccess (FlatLiteral _) (FlatRegex _)             = True
pathAccess _ _                                       = False

formatFiltersList :: [FlatFilter] -> Text
formatFiltersList filters = "[" <> T.intercalate "," (map renderFlatFilterUnquoted filters) <> "]"

runQuery1 :: [FlatRule] -> [FlatRule]
runQuery1 rules =
  [ rule
  | rule@(FlatRule Allow action filters) <- rules
  , action == "file-writeSTAR"
  , not (any isContainerFilter filters)
  , let caps = filter isCapability filters
  , all isThirdPartyCap caps
  ]

runQuery2 :: [FlatRule] -> [FlatRule]
runQuery2 rules =
  [ rule
  | rule@(FlatRule Allow action filters) <- rules
  , action `elem` readOperations
  , not (any isContainerFilter filters)
  , not (any isCapability filters)
  ]

runQuery3 :: [FlatRule] -> [(FlatRule, FlatRule)]
runQuery3 rules =
  [ (rRule, wRule)
  | rRule@(FlatRule Allow rAct rFilters) <- rules
  , rAct `elem` readOperations
  , not (any isContainerFilter rFilters)
  , not (any isCapability rFilters)
  , wRule@(FlatRule Allow wAct wFilters) <- rules
  , wAct `elem` writeOperations
  , not (any isContainerFilter wFilters)
  , not (any isCapability wFilters)
  , rFile <- filter isFileFilter rFilters
  , wFile <- filter isFileFilter wFilters
  , overlap rFile wFile
  ]

runQuery4 :: [FlatRule] -> [FlatRule]
runQuery4 rules =
  [ rule
  | rule@(FlatRule Allow action filters) <- rules
  , action `elem` readOperations
  , not (any isContainerFilter filters)
  , not (any isCapability filters)
  , hasMobileAccess filters
  ]
  where
    hasMobileAccess filters =
      let fileFilters = filter isFileFilter filters
          hasMobileFile = any checkMobilePath fileFilters
      in (not (null fileFilters) && hasMobileFile) || (null fileFilters && any isVnode filters)

    checkMobilePath (FlatSubpath p) = "/private/var/mobile/" `T.isPrefixOf` p
    checkMobilePath (FlatLiteral p) = "/private/var/mobile/" `T.isPrefixOf` p
    checkMobilePath _               = False

formatQuery1 :: FlatRule -> Text
formatQuery1 (FlatRule _ _ filters) = formatFiltersList filters

formatQuery2 :: FlatRule -> Text
formatQuery2 (FlatRule _ action filters) = action <> "," <> formatFiltersList filters

formatQuery3 :: (FlatRule, FlatRule) -> Text
formatQuery3 (FlatRule _ rAct rFilters, FlatRule _ wAct wFilters) =
  rAct <> "," <> formatFiltersList rFilters <> "," <> wAct <> "," <> formatFiltersList wFilters

formatQuery4 :: FlatRule -> Text
formatQuery4 (FlatRule _ action filters) = action <> "," <> formatFiltersList filters