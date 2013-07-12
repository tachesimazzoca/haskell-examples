module Example.Validator (
  Validator(..)
, validate
) where

data Validator
  = RequiredValidator
  | RangeLengthValidator (Maybe Int) (Maybe Int)
  deriving (Show)

-- |
-- >>> validate RequiredValidator []
-- Left "is required"
-- >>> validate RequiredValidator "foo"
-- Right True
-- >>> validate (RangeLengthValidator Nothing Nothing) []
-- Right True
-- >>> validate (RangeLengthValidator Nothing Nothing) "foo"
-- Right True
-- >>> validate (RangeLengthValidator (Just 2) Nothing) []
-- Left "must be at least 2 characters"
-- >>> validate (RangeLengthValidator (Just 2) Nothing) "foo"
-- Right True
-- >>> validate (RangeLengthValidator Nothing (Just 2)) []
-- Right True
-- >>> validate (RangeLengthValidator Nothing (Just 2)) "foo"
-- Left "must be at most 2 characters"
-- >>> validate (RangeLengthValidator (Just 1) (Just 3)) []
-- Left "must be between 1 and 3 characters"
-- >>> validate (RangeLengthValidator (Just 1) (Just 3)) "foo"
-- Right True
-- >>> validate (RangeLengthValidator (Just 1) (Just 3)) "fuga"
-- Left "must be between 1 and 3 characters"
validate :: Validator -> String -> Either String Bool
validate RequiredValidator [] = Left "is required"
validate RequiredValidator _  = Right True
validate (RangeLengthValidator Nothing Nothing) _ = Right True
validate (RangeLengthValidator (Just ln) Nothing) s
  = if length s >= ln then Right True
    else Left $ "must be at least " ++ show ln ++ " characters"
validate (RangeLengthValidator Nothing (Just mn)) s
  = if length s <= mn then Right True
    else Left $ "must be at most " ++ show mn ++ " characters"
validate (RangeLengthValidator (Just ln) (Just mn)) s
  = if l >= ln && l <= mn then Right True
    else Left $ "must be between " ++
      show ln ++ " and " ++ show mn ++ " characters" 
    where l = length s
