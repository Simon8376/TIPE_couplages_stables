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
  let r = reseau_make 1. 10 in 
  F.affiche_reseau r;
  let _ = F.node_init 1. r in 
  F.affiche_reseau r




let affiche_couplages r = 
  for i = 0 to F.len_reseau r -1 do 
    let node = F.nth_noeuds r i in
    Printf.printf "Couplage de %d\n" (F.id_n node);
    F.affiche_node_array (F.couplage node)
  done


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

  Printf.printf "------- Réseau et couplage après ajout: \n";
  F.affiche_reseau r;
  affiche_couplages r


let test_churn p t n c = (*période de churn t, nombre initial n de noeuds,
                  p est une proba de présence d'arête, c'est pas très important
                  *)
  for b = 1 to 3 do
    F.set_b b;
    for k = 0 to 39 do
      let r = reseau_make p n in 
      let f = Stdlib.open_out_gen [Open_append] 4 "time.txt" in
      F.protocol r;
      for i = 0 to 200 do 
        let t1 = Sys.time () in 
        let modified = F.node_add_rand p r in
        (try
          F.protocol r
        with Failure(s) ->  (*Echec, tant pis*)
          incr c);
        let t2 = Sys.time () in 
        if t2 -. t1 > t then 
          failwith "Echec: le churn time n'a pas laissé le temps de finir le calcul"
        else
          Printf.fprintf f "(%d, %d, %f, %d), \n" (F.get_b ()) (F.len_reseau r) (t2 -. t1) modified;
        flush_all ()
      done
    done
  done


let () = 
  let c = ref 0 in
  test_churn 1. 1. 10 c;
  Printf.printf "%d steps failed\nTotal time: %f" !c (Sys.time ())


(*
1. Ca termine plus
2. mettre un r0 pour pouvoir avoir satisfaction*)
