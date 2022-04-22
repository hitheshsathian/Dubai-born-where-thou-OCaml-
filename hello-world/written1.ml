type tree =
|Leaf
|Node of tree * int * tree;;

let rec sum_tree t =
  match t with
  | Leaf -> 0
  | Node (l, x, r) -> sum_tree l + x + sum_tree r;;

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
       if x = y then tree
       else if x < y then Node(insert l x, y, r)
       else Node(l, y, insert r x);;

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l;;

let tree = construct [2;1;3];;

let sumtailrec t =
  let rec aux acc ts = 
    match ts with
    |[] -> acc
    |h :: t -> match h with
      |Leaf -> aux acc t
      |Node(l,x,r) -> aux (acc+x) (t @ (l :: [r]))
  in aux 0 [t];;

let main () =
    let error_count = ref 0 in
    let _ =
      try
        assert (sumtailrec tree = sum_tree tree)
      with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string 
  e)^"\n")) in 
  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 10 programming questions are incorrect.\n") (!
error_count)
let _ = main()