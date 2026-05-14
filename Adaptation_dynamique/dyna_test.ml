module F = Protocol_dyna

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




let test_reseau_copy () = 
  let r0 = reseau_make 1. 5 in 
  let r = F.reseau_copy r0 in 

  Printf.printf "   ----- r0:\n";
  F.affiche_reseau r0;
  for i = 0 to F.len_reseau r0 -1 do 
    F.affiche_node_array (F.config (F.nth_noeuds r0 i))
  done;

  Printf.printf "\n   ----- Copy: \n";
  F.affiche_reseau r;
  for i = 0 to F.len_reseau r -1 do 
    F.affiche_node_array (F.config (F.nth_noeuds r i))
  done;

  F.node_del r0 (F.nth_noeuds r0 3);
  Printf.printf "\n  ------ r0 après modif de r0:\n";
  F.affiche_reseau r0;

  Printf.printf "\n ----- Copy après modif de r0:\n";
  F.affiche_reseau r;

  Printf.printf "\nLes pointeurs sont bien distincts\n\n"


let test_churn p t n regime c = (*période de churn t, nombre initial n de noeuds,
                regime est une liste qui alterne nombre de noeuds entrants/sortants
                  Exemple: [1; 3; 4; 2] signifie 1 entrant, puis 3 sortant, puis 4 entrant
                puis 2 sortant, puis on répète. Il y a toujours un laps de temps t entre 
                chaque opération
                  Idéalement 1 - 3 + 4 - 2 = 0
                  p est une proba de présence d'arête, c'est pas très important
                  Dans ce code, r0 est le réseau de référence, sur lequel on ajoute/retire
                les noeuds, puis on lance le protocol sur une copie de r0 (pour conserver
                les config)*)
  for k = 0 to 39 do
    for b = 1 to 3 do
      F.set_b b;
      let r0 = reseau_make p n in 
      let r = ref (F.reseau_copy r0) in  (*C'est le réseau sur lequel on travaille. A chaque fois qu'on fera une modif
                                on devra recommencer de r0 via un copy*)
      let f = Stdlib.open_out_gen [Open_append] 4 "time.txt" in
      F.protocol !r;
      let temp = ref regime in
      let e = ref 0 in
      for i = 0 to 400 do 
        if !temp = [] then begin (*Si la temp est écoulé on la redéfinit*)
          temp := regime;
          e := 0
        end
        else begin
          e := 1 - !e;
          let load = List.hd !temp in 
          temp := List.tl !temp;
          for i = 0 to load -1 do 
            let t1 = Sys.time () in 
            (if !e = 1 then begin (*On ajoute un noeud*)
              let _ = F.node_init p r0 in ()
            end
            else begin (*Sinon on en retire un*)
              let i = Random.int (F.len_reseau r0) in
              F.node_del r0 (F.nth_noeuds r0 i)
            end
            );
            r := F.reseau_copy r0;
            (try
              F.protocol !r
            with Failure(s) ->  (*Echec, tant pis*)
              incr c);
            let t2 = Sys.time () in 
            if t2 -. t1 > t then 
              failwith "Echec: le churn time n'a pas laissé le temps de finir le calcul"
            else
              Printf.fprintf f "(%d, %d, %f, %f), \n" (F.get_b ()) (F.len_reseau r0) (t2 -. t1) (F.stabilite r0 !r);
              flush_all ()
          done
        end
      done
    done
  done


let () = 
  let c = ref 0 in
  test_churn 1. 1. 10 [1] c;
  Printf.printf "%d steps failed\nTotal time: %f" !c (Sys.time ())

  (*let count_broken = ref 0 in
  let p = 0.5 in
  for b = 1 to 10 do
    F.set_b b;
    for n = 2 to 20 do
      for k = 0 to 40 do
        Printf.printf "Test no %d with %d nodes and b = %d\n\n" k (10*n) b;
        test_protocol p (10*n) count_broken
      done
    done;
  done;
  Printf.printf "%d boken" !count_broken*)



