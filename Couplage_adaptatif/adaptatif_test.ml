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


let test_copy () = 
  F.set_b 2;

  let r = reseau_make 1. 30 in 
  F.protocol r;
  let r1 = F.reseau_copy r in 
  F.affiche_reseau r;
  print_newline ();
  F.affiche_reseau r1


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


let test_churn p t n c = (*période de churn t, nombre initial n de noeuds,
                  p est une proba de présence d'arête, c'est pas très important
                  *)
    F.set_b 2;
      let r = ref (reseau_make p n) in 
      let f = Stdlib.open_out_gen [Open_append] 4 "time.txt" in
      F.affiche_reseau !r;
      F.protocol !r;
      for i = 0 to 200 do 
        Printf.printf "\n\n%d\n" i;
        let t1 = Sys.time () in 
        F.affiche_reseau !r;
        let r0 = F.reseau_copy !r in
        print_string "copied ok\n";
        F.affiche_reseau r0;
        let modified = F.node_add_rand p !r in
        print_string "added ok\n";
        F.affiche_unloving !r;
        Printf.printf "%d\n" modified;
        F.affiche_reseau !r;
        try
          F.protocol !r;
          let t2 = Sys.time () in 
          if t2 -. t1 > t then 
            failwith "Echec: le churn time n'a pas laissé le temps de finir le calcul"
          else
            Printf.fprintf f "(%d, %d, %f, %d), \n" (F.get_b ()) (F.len_reseau !r) (t2 -. t1) modified;
        with Failure(s) -> (
          incr c;
          Printf.printf "FAILED! %s" s;
          F.affiche_reseau !r;
          r := F.reseau_copy r0
        ) 
      done


let () = 

    test_node_add ()

  (*let c = ref 0 in
  test_churn 1. 1. 10 c;
  print_int !c*)


(*
1. Ca termine plus mais seulement rarement; trouver comment les oublier
2. Les len de config ne sont plus à jour après reseau_copy ou node_add
*)
