
module B = Blossom

let test_mate_unmate () = 
  let graphe = [|[2; 3]; [0; 2; 3]; [0; 1]; []|] in 
  let e = B.empty_matching graphe in
  assert(e = [|None; None; None; None|]); 
  B.mate e 1 2;
  assert(e = [|None; Some 2; Some 1; None|]); 
  B.unmate e 0;
  assert(e = [|None; Some 2; Some 1; None|]);
  B.unmate e 1;
  assert(e = [|None; None; None; None|])

let test_symdiff () = 
  let matching = [|None; Some 2; Some 1; None|] in 
  let path = [|Some 1; Some 0; Some 3; Some 2|] in 
  B.symdiff matching path;
  assert(matching = [|Some 1; Some 0; Some 3; Some 2|])


let test_augmenting_path () = 
  let g = [|[1; 2]; [0; 2]; [0; 1; 3]; [2]|] in 
  let matching = [|None; Some 2; Some 1; None|] in 
  let path = B.empty_matching g in 
  assert(B.augmenting_path g matching path);
  assert(path = [|Some 1; Some 0; Some 3; Some 2|])



let test_best_matching () = 
  let g = [|[1; 2]; [0; 2]; [0; 1; 3]; [2]|] in 
  assert(B.best_matching g = [|Some 1; Some 0; Some 3; Some 2|])


let test_bfs () = 
  let g = [|[1]; [0; 2]; [1; 3]; [2; 4]; [3]|] in 
  let matching = B.empty_matching g in 
  assert(B.composante g = [|0; 1; 0; 1; 0|]);
  let d = B.bfs g matching in 
  assert(d = [|0; 1; 0; 1; 0|]);
  let m2 = [|Some 1; Some 0; Some 3; Some 2; None|] in 
  assert(B.bfs g m2 = [|4; 3; 2; 1; 0|]);
  let m3 = [|None; Some 2; Some 1; None; None|] in 
  let d = B.bfs g m3 in 
  assert(d = [|0; 1; 2; 1; 0|])


let test_augmenting_path_bis () = 
  let g = [|[1]; [0; 2]; [1; 3]; [2; 4]; [3]|] in 
  let m1 = B.empty_matching g in 
  let path = B.empty_matching g in
  let d1 = B.augmenting_path_bis g m1 path in
  assert(d1);
  assert(path = [|None; Some 2; Some 1; Some 4; Some 3|]);
  let m2 = [|None; Some 2; Some 1; None; None|] in 
  let path = B.empty_matching g in
  let d2 = B.augmenting_path_bis g m2 path in
  assert(d2);
  assert(path = [|None; None; None; Some 4; Some 3|])