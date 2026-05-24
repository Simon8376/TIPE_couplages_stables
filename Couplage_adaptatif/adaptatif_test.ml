module F = Protocol_adaptatif

let crit = 1;; (*Nombre de noeuds qu'il faut ajouter/retirer avant
                de recalculer un couplage*)






let reseau_make p n = (* Construit un réseau. f est une fonction de construction d'une liste de préférence *)
  (*Printf.printf "-----    Création d'un réseau de taille %d   -----\n" n;*)
  let r = F.reseau_init () in 
  for i = 0 to n-1 do 
    let _ = F.node_init p r in 
    ()
  done;
  r



let test_ajout () = 
  let r = reseau_make 1. 60 in 
  F.affiche_reseau r;
  let _ = F.node_init 1. r in 
  F.affiche_reseau r




let affiche_couplages r = 
  for i = 0 to F.len_reseau r -1 do 
    let node = F.nth_noeuds r i in
    Printf.printf "Couplage de %d\n" (F.id_n node);
    F.affiche_node_array (F.couplage node) 0
  done


let est_trie r = 
  for i = 0 to F.len_reseau r -1 do 
    assert(F.est_trie (F.config (F.nth_noeuds r i)) 0)
  done


let test_copy () = 
  F.set_b 10;

  let r = reseau_make 1. 200 in 
  F.protocol r;
  let r1 = F.reseau_copy r in 
  F.affiche_reseau r;
  print_newline ();
  F.affiche_reseau r1;
  est_trie r1


let test_modif_lovers () = 
  let r = reseau_make 1. 10 in 
  F.protocol r;
  Printf.printf "------- Réseau et couplage après protocol: \n";
  F.affiche_reseau r;
    
  affiche_couplages r;

  let n = F.node_init 1. r in
  Printf.printf "------- Réseau après ajout:\n";
  F.affiche_reseau r;

  let _ = F.modif_lovers r n in 

  Printf.printf "------- Réseau et couplage après modif: \n";
  F.affiche_reseau r;
  affiche_couplages r


let test_node_add () = 
  let r = reseau_make 1. 60 in 

  for k = 0 to 2 do
    F.affiche_reseau r;
    F.protocol r;
    Printf.printf "------- Réseau et couplage après protocol: \n";
    F.affiche_reseau r;
      
    let _ = F.node_add_rand 1. r in
    Printf.printf "------- Réseau après ajout:\n";
    F.affiche_reseau r
  done


let test_node_del () = 
  F.set_b 1;
  let r = reseau_make 1. 20 in 
  F.protocol r;
  F.affiche_reseau r;
  let _ = F.node_add_rand 1. r in 
  F.affiche_reseau r;
  F.protocol r; 
  F.affiche_reseau r;
  let _ = F.node_del r (F.nth_noeuds r 0) in 
  F.affiche_reseau r;
  F.protocol r;
  F.affiche_reseau r


let mean liste = 
  let n = float_of_int (List.length liste) in
  if n = 0. then 0.
  else 
  (float_of_int (List.fold_left (fun acc v -> acc + v) 0 liste)) /. n


let test_churn_add p n c tot num_ajout = (*On ajoute num_ajout noeuds à un réseau de taille 
                                            initiale n
                                            p = 1., c = nbr d'échecs, tot = nbr de tours*)
    let f = Stdlib.open_out_gen [Open_append] 4 "time2.txt" in
    for b = 1 to 2 do 
      F.set_b b;
      let k = ref 0 in 
      while !k < 80 do
        try
          let r = ref (reseau_make p n) in 
          let values = Array.make (num_ajout +1) (0, 0, 0., 0, 0.) in
          let t1 = Sys.time () in
          F.protocol !r;
          let t2 = Sys.time () in
          values.(0) <- (F.get_b ()), (F.len_reseau !r), (t2 -. t1), 0, (F.satisfaction !r);
          for i = 1 to num_ajout do 
            est_trie !r;
            let t1 = Sys.time () in 
            let modified = F.node_add_rand p !r in
            F.protocol !r;
            let t2 = Sys.time () in 
            values.(i) <- (F.get_b ()), (F.len_reseau !r), (t2 -. t1), modified, (F.satisfaction !r)
          done;
          incr k;
          incr tot;
          F.retrieve_tmps !r;
          Printf.fprintf f "(%d, -1, -1.0, -1, %f), \n" (F.get_b ()) (mean (F.tmps_vie !r));
          Printf.printf "%d\n" !k;
          for i = 0 to num_ajout do 
            let v, w, x, y, z = values.(i) in
            Printf.fprintf f "(%d, %d, %f, %d, %f), \n" v w x y z 
          done;
        with _ -> (incr c; incr tot)
      done 
    done


let rec churn f p n c tot fin = 
  try
    let r = ref (reseau_make p n) in 
    let values = Array.make (2*fin) (' ', 0, 0, 0., 0, 0.) in
    for i = 0 to fin-1 do
      let modified = F.node_add_rand p !r in
      est_trie !r;
      let t1 = Sys.time () in
      F.protocol !r;
      let t2 = Sys.time () in
      values.(2*i) <- 'A', (F.get_b ()), n, (t2 -. t1), modified, (F.satisfaction !r);
      
      let k = Random.int (F.len_reseau !r) in 
      let modified = F.node_del !r (F.nth_noeuds !r k) in
      est_trie !r;
      let t1 = Sys.time () in
      F.protocol !r;
      let t2 = Sys.time () in 
      values.(2*i+1) <- 'R', (F.get_b ()), n, (t2 -. t1), modified, (F.satisfaction !r)
    done;
    incr tot;
    F.retrieve_tmps !r;
    Printf.fprintf f "('0', %d, %d, 0., 0, %f), \n" (F.get_b ()) n (mean (F.tmps_vie !r));
    for i = 0 to 2*fin-1 do 
      let c, v, w, x, y, z = values.(i) in
      Printf.fprintf f "('%c', %d, %d, %f, %d, %f), \n" c v w x y z;
    done
  with Failure(s) -> (
    incr tot;
    incr c;
    churn f p n c tot fin)



let test_churn_add_del p c tot fin = (*Ajout puis retrait de fin noeuds à un réseau
                        c = nbr d'échecs, tot = nbr de protocols lancés, p = 1.
                        fin = nbr de tours/2*)
    let f = Stdlib.open_out_gen [Open_append] 4 "time.txt" in
    for b = 1 to 2 do 
      F.set_b b;
      for n = 20 to 100 do 
        churn f p n c tot fin;
        Printf.printf "%d\n" n
      done 
    done


let () = 

  let fin = 30 in
  let c = ref 0 in
  let tot = ref 0 in
  test_churn_add_del 1. c tot fin;
  Printf.printf "%d failed out of %d\n" !c !tot


