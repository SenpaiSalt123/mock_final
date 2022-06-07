-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

module FileSystem where

-- | File system entry
data Entry = File String Int      -- File: name and size
           | Dir String [Entry]   -- Directory: name and content
  deriving (Eq, Show)

homedir = Dir "home"
            [ File "todo" 256
            , Dir  "HW0"
              [ File "Makefile" 575 ]
            , Dir  "HW1"
              [ File "Makefile" 845
              , File "HW1.hs"   3007
              ]
            ]

-- | Name of an entry
nameOf :: Entry -> String
nameOf (File name _) = name
nameOf (Dir name _)  = name

-------------------------------------------------------------------------------
-- Task 2.1: Tail-recursive size
-------------------------------------------------------------------------------

-- | Size of an entry; must be tail-recursive!
size :: Entry -> Int
size e = error "TBD"
  where
    loop :: Int -> [Entry] -> Int
    loop = error "TBD"

-------------------------------------------------------------------------------
-- Task 2.2: HOF Remove
-------------------------------------------------------------------------------

-- | Remove all contained entries that satisfy a predicate
remove :: (Entry -> Bool) -> Entry -> Entry
remove = error "TBD"
-------------------------------------------------------------------------------
-- Task 2.3: Cleanup
-------------------------------------------------------------------------------

-- | Remove empty contained entries
cleanup :: Entry -> Entry
cleanup = error "TBD"



