module Example.HTML (
  HTMLElement(..)
, Name
, OptionValue
, OptionLabel
, SelectOption
, SelectedOption
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

-- | Render HTML element.
--
-- >>> html (HTMLRadio "foo" [("1", "Yes"), ("0", "NO")] "1")
-- "<li><label><input type=\"radio\" name=\"foo\" value=\"1\" checked=\"checked\">Yes</label></li><li><label><input type=\"radio\" name=\"foo\" value=\"0\">NO</label></li>"
-- >>> html (HTMLCheckbox "foo" [("A", "A"), ("B", "B")] ["A", "B"])
-- "<li><label><input type=\"checkbox\" name=\"foo\" value=\"A\" checked=\"checked\">A</label></li><li><label><input type=\"checkbox\" name=\"foo\" value=\"B\" checked=\"checked\">B</label></li>"
-- >>> html (HTMLSelect "foo" [("A", "A"), ("B", "B")] ["A"])
-- "<select name=\"foo\"><option value=\"A\" selected=\"selected\">A</option><option value=\"B\">B</option></select>"
html :: HTMLElement -> String
html (HTMLRadio nm xs sel) = foldl
  (\acc (k, v) -> acc ++ format' (nm, k, v, k == sel)) "" xs
  where
    format' :: (Name, OptionValue, OptionLabel, Selected) -> String
    format' (s, k, v, y) =
      "<li><label><input type=\"radio\" " ++
      "name=\"" ++ s ++ "\" value=\"" ++ k ++ "\"" ++
      (if y then " checked=\"checked\"" else "") ++ ">" ++ v ++ "</label></li>"

html (HTMLCheckbox nm xs sels) = foldl
  (\acc (k, v) -> acc ++ format' (nm, k, v, (k `elem` sels))) "" xs
  where
    format' :: (Name, OptionValue, OptionLabel, Selected) -> String
    format' (s, k, v, y) =
      "<li><label><input type=\"checkbox\" " ++
      "name=\"" ++ s ++ "\" value=\"" ++ k ++ "\"" ++
      (if y then " checked=\"checked\"" else "") ++ ">" ++ v ++ "</label></li>"

html (HTMLSelect nm xs sels) = (foldl
  (\acc (k, v) -> acc ++ format' (k, v, (k `elem` sels)))
  ("<select name=\"" ++ nm ++ "\">") xs) ++ "</select>"
  where
    format' :: (OptionValue, OptionLabel, Selected) -> String
    format' (k, v, y) =
      "<option value=\"" ++ k ++ "\"" ++
      (if y then " selected=\"selected\"" else "") ++ ">" ++ v ++ "</option>"
