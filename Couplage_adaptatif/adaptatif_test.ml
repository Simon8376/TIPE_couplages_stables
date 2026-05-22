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
  Printf.printf "-----    Fin création du réseau    -----\n\n";
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


let test_churn p t n c tot = (*période de churn t, nombre initial n de noeuds,
                  p est une proba de présence d'arête, c'est pas très important
                  *)
    let f = Stdlib.open_out_gen [Open_append] 4 "time.txt" in
    for b = 1 to 2 do 
      F.set_b b;
      let k = ref 0 in 
      while !k < 80 do
        Printf.printf "%d\n" !k;
        try
          let r = ref (reseau_make p n) in 
          let values = Array.make 201 (0, 0, 0., 0, 0.) in
          let t1 = Sys.time () in
          F.protocol !r;
          let t2 = Sys.time () in
          values.(0) <- (F.get_b ()), (F.len_reseau !r), (t2 -. t1), 0, (F.satisfaction !r);
          for i = 1 to 200 do 
            est_trie !r;
            let t1 = Sys.time () in 
            let modified = F.node_add_rand p !r in
            F.protocol !r;
            let t2 = Sys.time () in 
            if t2 -. t1 > t then 
              failwith "Echec: le churn time n'a pas laissé le temps de finir le calcul"
            else 
              values.(i) <- (F.get_b ()), (F.len_reseau !r), (t2 -. t1), modified, (F.satisfaction !r)
          done;
          incr k;
          incr tot;
          for i = 0 to 200 do 
            let v, w, x, y, z = values.(i) in
            Printf.fprintf f "(%d, %d, %f, %d, %f), \n" v w x y z 
          done;
        with _ -> (incr c; incr tot)
      done 
    done


let () = 

  let c = ref 0 in
  let tot = ref 0 in
  test_churn 1. 1. 10 c tot;
  Printf.printf "%d failed out of %d\n" !c !tot


(*
1. Ca termine plus mais seulement rarement; trouver comment les oublier
2. Les len de config ne sont plus à jour après reseau_copy ou node_add
*)
