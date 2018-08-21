
open Core

type 'a tree_mut =
    EmptyM
  | SingleM of 'a
  | BranchM of
      {ele : 'a;
       l   : ('a tree_mut) ref;
       r   : ('a tree_mut) ref;
      }


type 'a tree =
    Empt
  | Single of 'a
  | Branch of
      { ele : 'a;
        l   : 'a tree;
        r   : 'a tree;
      }

exception Empty


let rec purify_tree = function
    EmptyM    -> Empt
  | SingleM x -> Single x
  | BranchM {l ; ele ; r} -> Branch {l = (purify_tree !l);
                                     ele = ele;
                                     r = (purify_tree !r)}

module Tree = struct

  type 'a t = 'a tree
  let ele_exn = function
      BranchM {ele} -> ele
    | SingleM ele   -> ele
    | EmptyM        -> raise Empty

  let from_list_bredth = function
      [] -> Empt
    | x :: xs ->
       let queue = (Deque.create ()) in
       let root  = ref (SingleM x) in
       Deque.enqueue_back queue root;

       let rec build = function
           []  -> ()
         | [x] -> let node = Deque.dequeue_front_exn queue in
                  node := BranchM { l   = ref (SingleM x);
                                    ele = ele_exn !node;
                                    r   = ref EmptyM}
         | x :: y :: xs -> let node = Deque.dequeue_front_exn queue in
                           let l = ref (SingleM x) in
                           let r = ref (SingleM y) in
                           Deque.enqueue_back queue l;
                           Deque.enqueue_back queue r;
                           node := BranchM {l = l;
                                            ele = ele_exn !node;
                                            r = r};
                           build xs

       in
       build xs;
       purify_tree !root

end
