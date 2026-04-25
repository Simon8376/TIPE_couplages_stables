module F = Protocol_simon



let reseau_make n = (* Test de la convergence du protocol *)
  (*Printf.printf "-----    Création d'un réseau de taille %d   -----\n" n;*)
  let r = F.reseau_init_cycles n in 
  F.affiche_reseau r;
  Printf.printf "-----    Fin création du réseau    -----\n\n";
  r

let reseau_make_2 n = (* Test de la convergence du protocol *)
  Printf.printf "-----    Création d'un réseau uniformisé de taille %d   -----\n" n;
  let r = F.reseau_init () in 
  for i = 0 to 2*n-1 do 
    let _ = F.node_init r in 
    ()
  done;
  for i = 0 to n-1 do 
    F.node_del r (F.nth_noeuds r i)
  done;
  F.affiche_reseau r;
  Printf.printf "-----    Fin création du réseau    -----\n\n";
  r


exception Boom

let test_pfile s =
  let r = reseau_make 50 in 
  Printf.printf "-----    Début test_pfile - type %s   -----\n" s;
  Printf.printf "Taille du réseau: %d\n" (F.len_reseau r);
  Printf.printf "Réseau ainsi crée:\n";
  F.affiche_reseau r;
  print_newline ();

  if s = "couplage" || s = "all" then F.protocol_vtest r;

  Printf.printf "\n---    Début test des pfiles - type %s  --------\n" s;
    for i = 0 to F.len_reseau r -1 do 
      if s = "couplage" && F.est_trie (F.couplage (F.nth_noeuds r i)) 
        || s = "config" && F.est_trie (F.config (F.nth_noeuds r i)) 
        || s = "all" && F.est_trie (F.couplage (F.nth_noeuds r i)) && F.est_trie (F.config (F.nth_noeuds r i)) then 
        Printf.printf "Noeud %d: pfile ok\n" i
      else
        Printf.printf "Noeud %d: PFILE PAS OK\n" i ;
    done;


  Printf.printf "-----    Fin test_pfile    -----\n\n\n"


let test_acyclisme () = 
  let r = reseau_make 500 in 

  let rec parcours_prof vus node prev = 
    if not vus.(F.lr (F.noeud node)) then begin
      vus.(F.lr (F.noeud node)) <- true;
      let m = F.best_config (F.noeud node) in 
      let b = if F.id (Some m) != F.id (Some prev) then parcours_prof vus m node else true in 
      b
    end
    else
      false 
  in

  for i = 0 to 499 do 
    let vus = Array.make 500 false in
    if not (parcours_prof vus (F.best_config (F.nth_noeuds r i)) (F.best_config (F.nth_noeuds r i))) then begin
      Printf.printf "Found a cycle!\n";
      exit(1)
    end
  done;
  Printf.printf "No cycle!\n\n"

let test_protocol n c = 
  Printf.printf "-----    Début test_protocol   -----\n";
  let r = reseau_make n in 
  print_string "Début du protocol:\n";
    F.protocol r;
    print_string "\n\nRéseau à la fin du protocol: \n";
    F.affiche_reseau r;
    print_newline ();
    print_string "Couplage à la fin:\n";
    for i = 0 to F.len_reseau r -1 do 
      let n = F.nth_noeuds r i in 
        Printf.printf "Node %d: " (F.lr n); 
        F.affiche_node_array (F.couplage n)
    done;

    Printf.printf "-----    Fin test_protocol   -----\n\n\n"






let test_trileen () = 
  let r = reseau_make 10 in 
  F.protocol r;
  for i = 0 to F.len_reseau r -1 do 
    let n = F.nth_noeuds r i in 
    Printf.printf "\n\nNode %d: \n        " (F.lr n); 
    F.affiche_node_array (F.config n);
    Printf.printf "\n       ";
    F.affiche_node_array (F.couplage n);
  done


let () = 
  let count_broken = ref 0 in
  test_protocol 10 count_broken;
  Printf.printf "%d boken" !count_broken



(*
Bien refaire les tests et le protocol
Piste d'amélioration: quand tu changes les flag le long d'un
  chemin tu fais des changements systématiquement surtout au debut
  on pourrait faire les chemins récursivement apres l'étude du chemin
C'est un casse tete, il y a des erreurs de flag
  *)

