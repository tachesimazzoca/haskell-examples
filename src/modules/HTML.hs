{-
  * HTML

    ghci> (HTMLRadio "foo" [("1", "Yes"), ("0", "NO")] "1")
    HTMLRadio "foo" [("1","Yes"),("0","NO")] "1"
    ghci> html (HTMLRadio "foo" [("1", "Yes"), ("0", "NO")] "1")
    "<li><label><input type=\"radio\" name=\"foo\" value=\"1\" checked=\"checked\"></label></li>...
    ghci> html (HTMLCheckbox "foo" [("A", "A"), ("B", "B")] ["A", "B"])
    "<li><label><input type=\"checkbox\" name=\"foo\" value=\"A\" checked=\"checked\"></label></li>...
    ghci> html (HTMLSelect "foo" [("A", "A"), ("B", "B")] ["A"])
    "<select name=\"foo\"><option value=\"A\" selected=\"selected\">A</option>....
-}

module HTML (
  HTMLElement(..)
, html
) where

type Name = String
type OptionValue = String
type OptionLabel = String
type SelectOption = (OptionValue, OptionLabel)
type SelectedOption = String
type Selected = Bool

data HTMLElement
  = HTMLRadio    Name [SelectOption] SelectedOption
  | HTMLCheckbox Name [SelectOption] [SelectedOption]
  | HTMLSelect   Name [SelectOption] [SelectedOption]
  deriving (Show)

html :: HTMLElement -> String
-- HTMLRadio
html (HTMLRadio nm xs sel) = foldl
  (\acc (k, v) -> acc ++ format' (nm, k, v, k == sel)) "" xs
  where
    format' :: (Name, OptionValue, OptionLabel, Selected) -> String
    format' (nm, k, v, y) =
      "<li><label><input type=\"radio\" " ++
      "name=\"" ++ nm ++ "\" value=\"" ++ k ++ "\"" ++
      (if y then " checked=\"checked\"" else "") ++ "></label></li>"

-- HTMLCheckbox
html (HTMLCheckbox nm xs sels) = foldl
  (\acc (k, v) -> acc ++ format' (nm, k, v, (k `elem` sels))) "" xs
  where
    format' :: (Name, OptionValue, OptionLabel, Selected) -> String
    format' (nm, k, v, y) =
      "<li><label><input type=\"checkbox\" " ++
      "name=\"" ++ nm ++ "\" value=\"" ++ k ++ "\"" ++
      (if y then " checked=\"checked\"" else "") ++ "></label></li>"

-- HTMLSelect
html (HTMLSelect nm xs sels) = (foldl
  (\acc (k, v) -> acc ++ format' (k, v, (k `elem` sels)))
  ("<select name=\"" ++ nm ++ "\">") xs) ++ "</select>"
  where
    format' :: (OptionValue, OptionLabel, Selected) -> String
    format' (k, v, y) =
      "<option value=\"" ++ k ++ "\"" ++
      (if y then " selected=\"selected\"" else "") ++ ">" ++ v ++ "</option>"
