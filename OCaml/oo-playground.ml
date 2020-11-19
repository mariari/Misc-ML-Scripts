
class test = object(s)
  method trueth = true
  method on_truth =
    if s#trueth
    then 3
    else 4
end

class inherets = object
  inherit test
  method trueth = false
end

let foo p q =
  p#on_truth + q#on_truth

class test2 = object(s)
  method private trueth = true
  method on_truth =
    if s#trueth
    then 3
    else 4
end

class inherets2 = object
  inherit test
  method trueth = false
end

 
