module PrintF

%access public export

data Formatting
  = FInt    Formatting
  | FString Formatting
  | FOther  Char Formatting
  | FEnd

%default total

format : List Char -> Formatting
format ('%' :: 'i' :: xs) = FInt (format xs)
format ('%' :: 's' :: xs) = FString (format xs)
format (x :: xs)          = FOther x (format xs)
format []                 = FEnd

-- Idris doesn't view this as total by default ☹
-- format : List Char -> Formatting
-- format xs =
--   let pred = (/= '%') in
--   case (takeWhile pred xs, dropWhile pred xs) of
--     (xs, '%' :: 'i' :: ys) => other xs (FInt (format ys))
--     (xs, '%' :: 's' :: ys) => other xs (FString (format ys))
--     (xs, '%' :: ys)        => other (xs ++ ['%']) (format ys)
--     (xs, [])               => other xs FEnd
--     (xs, ys)               => other xs (format ys)
--   where
--     other : List Char -> Formatting -> Formatting
--     other zs z = FOther (pack zs) z

interpFormat : Formatting -> Type
interpFormat (FInt f)     = Int -> interpFormat f
interpFormat (FString f)  = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd         = String

formatString : String -> Formatting
formatString s = format (unpack s)

formatF : (s : String) -> interpFormat (formatString s)
formatF xs = rec (formatString xs) []
  where
    rec : (fmt : Formatting) -> List String -> interpFormat fmt
    rec (FInt x)     st = \i => rec x (show i :: st)
    rec (FString x)  st = \s => rec x (s :: st)
    rec (FOther x y) st =       rec y (singleton x :: st)
    rec FEnd         st =       foldr (++) "" (reverse st)

test : String
test = formatF "%i how do you do %s" 3 "d"
