open Core

type 'a tree_mut =
    EmptyM
  | SingleM of 'a
  | BranchM of
      { ele : 'a;
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
  | BranchM x -> Branch { l   = purify_tree !(x.l);
                          ele = x.ele;
                          r   = purify_tree !(x.r)
                        }

module Tree = struct

  type 'a t = 'a tree

  let ele_exn = function
      BranchM {ele} -> ele
    | SingleM ele   -> ele
    | EmptyM        -> raise Empty

  let from_list_bredth = function
      [] -> Empt
    | x :: xs ->
       let queue = Queue.create () in
       let root  = ref (SingleM x) in
       Queue.enqueue queue root;

       let rec build = function
           []  -> ()
         | [x] -> let node = Queue.dequeue_exn queue in
                  node := BranchM { l   = ref (SingleM x);
                                    ele = ele_exn !node;
                                    r   = ref EmptyM
                                  }
         | x :: y :: xs -> let node = Queue.dequeue_exn queue in
                           let l = ref (SingleM x) in
                           let r = ref (SingleM y) in
                           Queue.enqueue_all queue [l; r];
                           node := BranchM { l = l;
                                             ele = ele_exn !node;
                                             r = r;
                                           };
                           build xs
       in
       build xs;
       purify_tree !root

end
