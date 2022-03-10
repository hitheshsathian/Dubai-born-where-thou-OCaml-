open List

let rec cond_dup l f = 
  match l with
  | [] -> []
  | h :: t -> if f(h) then (h :: [h]) @ (cond_dup t f) else h :: cond_dup t f;;
assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5])

let rec n_times (f, n, v) =
  if n <= 0 then v else n_times(f, n-1, f(v));;
assert (n_times((fun x-> x+1), 50, 0) = 50)

let rec zipwith f l1 l2 =
  match l1, l2 with
  | [],_ -> []
  | _,[] -> []
  | h1 :: t1, h2 :: t2 -> f h1 h2 :: zipwith f t1 t2;;
assert (zipwith (+) [1;2;3] [4;5] = [5;7])

let buckets p l =
[]

let fib_tailrec n =
  let rec helper prev cur counter = 
    match counter with
    | 0 -> prev
    | n -> if n>0 then helper cur (cur + prev) (counter -1) else 0
  in helper 0 1 n;;
assert (fib_tailrec 50 = 12586269025)

let rec assoc_helper lst elem =
  if lst = [] then [(elem,1)] else
    match lst with
    |[] -> []
    |(h,x) :: t -> if elem =h then (h, x+1) :: t else (h,x) :: assoc_helper t elem;;

let assoc_list lst = List.fold_left assoc_helper [] lst
(*assoc_list [1; 2; 2; 1; 3] = [(2,2); (1, 2); (3, 1)] *)

let ap fs args = 
  List.fold_left (fun x y -> x@y) [] (List.map (fun x -> (List.map (fun y -> x y) args)) fs);;
assert (ap [(fun x -> x^"?"); (fun x -> x^"!")] ["foo";"bar"] = ["foo?";"bar?";"foo!";"bar!"])

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l


let rec fold_inorder f acc t =
  match t with
  | Leaf -> acc
  | Node(t1, v, t2) -> fold_inorder f (f(fold_inorder f acc t1) v) t2;;
assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);;
assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)

let levelOrder t =
   let rec depth t =
    match t with
    | Leaf -> 0
    | Node(t1,v,t2) -> 
        let left = depth t1 in
        let right = depth t2 in
        if left >= right then 1 + left else 1 + right in 
        let depth = depth t in
        let rec level t x y acc = 
          match t with
        | Leaf -> acc
        | Node(t1,v,t2) -> if x <> y then level t1 (x+1) y (level t2 (x+1) y acc) else v :: acc in
        let rec tree t lt acc = 
          match t with
          | Leaf -> acc
          | Node(t1,v,t2) ->if lt <> depth then (level t 0 lt []) :: (tree t (lt+1) acc) else acc in
          tree t 0 [];;
      
assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])

let rec min t =
  match t with
  | Leaf -> 0
  | Node(Leaf,v,Leaf) -> v
  | Node(t1,v,t2) -> min t1;;

  let rec remove x t =
    match t with 
    | Leaf -> Leaf
    | Node (t1, v, t2) -> 
    if x = v then
    match t with
    |Leaf -> Leaf
    | Node (Leaf, _, t2) -> t2
    | Node (t1, _, Leaf) -> t1
    | Node (t1, _, t2) ->
    let newv = min t2 in
    let newt2 = remove newv t2 in
    Node (t1, newv, newt2)
    else if x < v then 
    Node (remove x t1, v, t2)
    else  
    Node (t1, v, remove x t2);;

assert (remove 50 (Node (Node (Leaf, 40, Leaf), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
  = (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 70, Node (Leaf, 80, Leaf)))))




(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5])
      (* BEGIN HIDDEN TESTS *)
      ; assert (cond_dup [] (fun x -> x mod 2 = 1) = []);
      assert (cond_dup [1;2;3;4;5] (fun x -> x mod 2 = 0) = [1;2;2;3;4;4;5])
      (* END HIDDEN TESTS *)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50)
      (* BEGIN HIDDEN TESTS *)
      ; assert (n_times ((fun x->x+1), 0, 1) = 1);
      assert (n_times((fun x-> x+2), 50, 0) = 100)
      (* END HIDDEN TESTS *)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert ([5;7] = (zipwith (+) [1;2;3] [4;5]))
      (* BEGIN HIDDEN TESTS *)
      ; assert ([(1,5); (2,6); (3,7)] = (zipwith (fun x y -> (x,y)) [1;2;3;4] [5;6;7]))
      (* END HIDDEN TESTS *)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
      (* BEGIN HIDDEN TESTS *)
      ; assert (buckets (fun x y -> (=) (x mod 2) (y mod 2)) [1;2;3;4;5;6] = [[1; 3; 5]; [2; 4; 6]])
      (* END HIDDEN TESTS *)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025)
      (* BEGIN HIDDEN TESTS *)
      ; assert (fib_tailrec 90 = 2880067194370816120)
      ; assert (fib_tailrec 0 = 0)
      (* END HIDDEN TESTS *)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in


  (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for levelOrder *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove *)
  let _ =
    try
      assert (remove 20 (Node (Node (Node (Leaf, 20, Leaf), 30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
                      = (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 30 (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
                      = (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 50 (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
                      = (Node (Node (Leaf,                  40, Leaf                 ), 60, Node (Leaf,                  70, Node (Leaf, 80, Leaf)))))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 10 programming questions are incorrect.\n") (!error_count)

let _ = main()
