module Fun

str_index' : (i : Nat) -> (s : String) -> {auto p : LT i (length s)} -> Char
str_index' i s {p} = assert_total $ prim__strIndex s (toIntNat i)

getYes : (res : Dec p) -> case res of { Yes _ => p ; No _ => () }
getYes (Yes prf)   = prf
getYes (No contra) = ()

test : IO ()
test = do
  s <- getLine
  case isLTE (1 + 10) (length s) of
    Yes p => do
      let i = lteTransitive (getYes (isLTE 3 11)) p
      -- we don't have to pass i
      print (str_index' 2 s {p = i})
    No _ =>
      print "sad"
