


let max_noeuds = 1000;; (*Nombre max d'utilisateurs*)
let max_marque = 100000;; (*La qualité d'une connexion est évaluée entre 0 et 1000*)
let max_config = 1000;; (*Degré maximal d'un sommet dans le graphe d'acceptance. ATTENTION, CHANGER CA POURRAIT CASSER LE CODE *)
let b = ref 1;; (*Nombre d'arêtes du couplage incidentes à chaque sommet: on cherche un b-couplage*)

Random.init (int_of_float (Sys.time () *. 100000000.));


type num = {mutable num : int;} (*Permet de définit l'id d'un nouveau noeud*)

let num = {num = 0};;


type trileen = Unmatched | Unloving | Loving (*Différents états d'un noeud au sens d'une configuration / d'un couplage*)

type data = {
  cycles_coupe : int; (*Nombre de cycles coupés*)
  noeuds_del : int; (*Nombre de noeuds effacés au cours du processus*)
}


type node = {
  id: int;
  mutable ind_noeuds : int; (*indice du node dans le tableau reseau.noeuds *)
  mutable ind_unloving : int; (*indice du node dans reseau.unloving*)
  config : pfile; (* Tableau de noeuds rangé par marque décroissante des nodes avec lesquels le node peut échanger des données.
    C'est la liste d'adjacence de node dans le graphe d'acceptance.
    Les cases non utilisées sont à la fin du tableau et à None*)
  couplage : pfile; (*Tableau de taille b ordonné par marque décroissante des nodes intervenant dans le couplage*)
  mutable num_loving : int; (*Nombre (majoré par b et par couplage.len) de peers au sein de couplage qui sont loving*)
}
and pfile = { (*Tableau trié selon la marque décroissante. Les cases non utilisées sont à None*)
  mutable tab : champ option array;
  mutable len : int;
}
and champ = { (*Structure modélisant une arête depuis un noeud*)
  n : node;
  e : trileen; (*indique si node est dans le couplage ou pas, relevant seulement pour config*)
  marque : int;
}

type reseau = {
  mutable noeuds : node option array; (*tableau des nodes dans le réseau. Le node d'indice node.id est à l'indice node.id*)
  mutable len_noeuds : int; (*Taille de noeuds*)
  mutable len_unloving : int; (*Taille de unloving. Le protocol s'arrête quand ça atteint 0*)
  mutable unloving : node option array; (*tableau des noeuds dont le couplage n'est pas composé que de peers loving. Idem niveau node.id *)
  mutable data : data; (*Stat intéressantes*)
}

exception Ok


let rec pfile_vide = {
  tab = Array.make 0 None;
  len = 0;
}
and noeud_vide = {
  id = 0;
  ind_noeuds = max_noeuds -1;
  ind_unloving = max_noeuds -1;
  config = pfile_vide;
  couplage = pfile_vide;
  num_loving = 0;
}

(*  ------    Tableau dynamique    ------   *)


let id node = 
  match node with 
  |  None -> -2
  |  Some c -> c.n.id


let tril_to_string e =
  match e with 
  |  Unmatched -> "unmatched"
  |  Loving -> "loving"
  |  Unloving -> "matched unloving"

let affiche_node_array pfile = 
  try
    for i = 0 to Array.length pfile.tab -1 do 
      match pfile.tab.(i) with 
      |  None -> (Printf.printf "None, "; raise Ok)
      |  Some c -> 
          match c.n.couplage.tab.(if c.n.couplage.len -1 >= 0 then c.n.couplage.len -1 else 0) with 
          |  None ->  Printf.printf "Some %d with marque %d is alone, " c.n.id c.marque
          |  Some best when c.n.num_loving >= 1 -> Printf.printf "Some %d (%s) with marque %d is loving with %d, " c.n.id (tril_to_string c.e) c.marque best.n.id
          |  Some best -> Printf.printf "Some %d (%s) with marque %d is coupled with %d, " c.n.id (tril_to_string c.e) c.marque  best.n.id
    done;
    print_newline ()
  with Ok -> print_newline ()



let agrandit_tab tab len =
  if len < Array.length tab then tab
  else
    let newtab = Array.make (2 * len + 1) None in
    Array.blit tab 0 newtab 0 len;
    for i = 0 to len-1 do 
      if newtab.(i) = None then failwith "Il y a un none au milieu la ou il devrait pas etre"
    done;
    newtab



(*  ----- Implémentation de la file de priorité - NAN -----   *)

let switch pfile i j = 
  if i >= pfile.len || j >= pfile.len then failwith "Erreur, switch sur des indices interdits"
  else begin
    match pfile.tab.(i), pfile.tab.(j) with 
    |  ni, nj ->
      pfile.tab.(i) <- nj;
      pfile.tab.(j) <- ni
  end




let has_before pfile node n = (*Verifie si node apparait dans pfile dans les indices [0, len - n]*)
  try
    for i = 0 to pfile.len -1 - n do 
      match pfile.tab.(i) with 
      |  Some c -> 
      if c.n.id = node.id then 
        raise Ok
      |  None -> ()
    done;
    false
  with Ok -> true
    




let pfile_insere pfile node e marque = 
  pfile.tab <- agrandit_tab pfile.tab pfile.len;
  pfile.len <- pfile.len +1;
  let i = ref (pfile.len -2) in
  let b = ref true in
  while !b && !i >= 0 do 
    match pfile.tab.(!i) with 
    |  None -> failwith "Un none au milieu"
    |  Some c when c.marque > marque -> 
      switch pfile !i (!i+1);
      i := !i -1
    |  Some c -> 
      b := false
  done;
  pfile.tab.(!i+1) <- Some {n = node; e = e; marque = marque}




let pfile_defile pfile = (*Renvoie et retire le plus grand élément de la pfile*)
  if pfile.len > Array.length pfile.tab then failwith "Erreur dans defile, la len est plus grande que la taille de l'array"
  else if pfile.len = 0 then failwith "Défilement sur une pfile vide"
  else
    let n = pfile.tab.(pfile.len -1) in 
    pfile.tab.(pfile.len -1) <- None;
    pfile.len <- pfile.len -1;
    n



let est_trie pfile = 
  try
    for i = 0 to pfile.len -2 do 
      match pfile.tab.(i), pfile.tab.(i+1) with 
      |  None, _ | _, None -> failwith "None au milieu"
      |  Some c1, Some c2 ->
        if c1.marque > c2.marque then raise Not_found
    done;
    true
  with Not_found -> false



(*MISC*)

let worst node = 
  node.couplage.tab.(0)


let empty_pfile () = 
  {
    len = 0;
    tab = Array.make 1 None;
  }

let rec affiche_liste_int liste = 
  match liste with 
  |  [] -> Printf.printf "Vide\n"
  |  [h] -> Printf.printf "%d\n" h
  |  h :: t -> 
    Printf.printf "%d, " h;
    affiche_liste_int t


let randint borne = 
  if borne <= 0 then
    0 
  else
    Random.int borne



(*  -------   Initialisation d'un nouveau node    --------   *)


let knuth_fischer_yates tab = 
  for i = 0 to Array.length tab -1 do 
    let j = randint (i+1) in
    if j < i then (
      let k = tab.(i) in 
      tab.(i) <- tab.(j);
      tab.(j) <- k
    )
  done

let nth_unloving reseau i = 
  match reseau.unloving.(i) with 
  |  None -> noeud_vide
  |  Some n -> n





let connections_init reseau =  (*En première approche les configuration sont choisies au hasard;
                           en vrai, elles dépendent des paquets disponibles.
                           On suppose nb << len_NOEUDS. On est alors en 0(len_NOEUDS)*)
  if reseau.len_unloving <= 1 then begin
    (*Printf.printf "Pas assez de nœuds unloving disponibles";*)
    {
      len = 0;
      tab = Array.make max_config None;
    }
  end
  else begin
    let n = 1 + randint (min (reseau.len_unloving - 1) max_config) in
    let config = Array.make reseau.len_unloving None in
    (*Printf.printf "Adding %d nodes ... " n;*)
    
    for i = 0 to reseau.len_unloving - 1 do 
      config.(i) <- reseau.unloving.(i)
    done;

    knuth_fischer_yates config;

    let pf = {
      len = 0;
      tab = Array.make max_config None;
    } in 

    for i = 0 to min (n-1) (reseau.len_unloving-1) do 
      let m = randint max_marque in
      match config.(i) with 
      | None -> failwith "Has None ... "
      | Some nod -> (
          (*Printf.printf "Adding %d ... " nod.id;*)
          pfile_insere pf nod Unmatched m)
    done;
    pf
  end



let connections_init_complet reseau = 
  let pf ={
    tab = Array.make max_config None;
    len = 0
  } in
  for i = 0 to reseau.len_unloving -1 do 
    match reseau.unloving.(i) with 
    |  None -> failwith "None au milieu du unloving"
    |  Some node -> 
      let m = randint max_marque in
      pfile_insere pf node Unmatched m;
  done;
  pf 

    
  

let node_init reseau = 
  let pf = connections_init_complet reseau in
  let node = {
    id = num.num;
    ind_noeuds = reseau.len_noeuds;
    ind_unloving = reseau.len_unloving;
    config = pf;
    couplage = {
      tab = Array.make !b None;
      len = 0;
    };
    num_loving = 0;
  } 
  in 
  num.num <- num.num +1;
  
  (*il faut ensuite ajouter n aux ensemble de configuration de tous les autres pour symétrie*)
  for i = 0 to node.config.len -1 do 
    match node.config.tab.(i) with
    |  None -> failwith "Erreur dans le len de la config qui ne correspond pas aux Some / None"
    |  Some c -> 
      pfile_insere c.n.config node Unmatched c.marque
  done;
      

  (*Et ajouter n à la liste des sommets*)
  reseau.len_noeuds <- reseau.len_noeuds +1;
  reseau.noeuds.(node.ind_noeuds) <- Some node;

  reseau.len_unloving <- reseau.len_unloving +1;
  reseau.unloving.(node.ind_unloving) <- Some node;

  node


let node_init_noedge r = 
  r.len_unloving <- r.len_unloving +1;
  r.len_noeuds <- r.len_noeuds +1;
  num.num <- num.num +1;
  {
    id = num.num;
    ind_noeuds = r.len_noeuds -1;
    ind_unloving = r.len_unloving -1;
    config = {
      tab = Array.make max_config None;
      len = 0;
    };
    couplage = {
      tab = Array.make !b None;
      len = 0;
    };
    num_loving = 0;
  }


let reseau_init_noedge n =
  let noeuds = Array.make max_noeuds None in
  let unloving = Array.make max_noeuds None in
  let r = {
    noeuds = noeuds;
    len_noeuds = 0;
    unloving = unloving;
    len_unloving = 0;
    data = {
      cycles_coupe = 0;
      noeuds_del = 0;};
  } in 
  for i = 0 to n-1 do
    let n = node_init_noedge r in 
    r.noeuds.(i) <- Some n;
    r.unloving.(i) <- Some n;
  done;
  r


let reseau_init_cycles t = (*Construit un graphe orienté (!!) donc avec potentiellement cyclique
                              de manière aléatoire, avec n noeuds*)
  let reseau = reseau_init_noedge t in
  for i = 0 to t-1 do
    match reseau.noeuds.(i) with
    |  None -> failwith "Ne devrait pas arriver"
    |  Some node -> 
      for j = 0 to t-1 do 
        match reseau.noeuds.(j) with 
        |  None -> failwith "None"
        |  Some peer when peer.id != node.id -> 
          pfile_insere node.config peer Unmatched (randint max_marque)
        | _ -> ()
      done
  done;
  reseau



let nth_noeuds reseau i = 
  if i >= reseau.len_noeuds then failwith "Too big!"
  else
  match reseau.noeuds.(i) with 
  |  None -> noeud_vide
  |  Some n -> n


let remove_nth pfile j =
  for i = j to pfile.len -2 do 
    switch pfile i (i+1)
  done;
  pfile.tab.(pfile.len -1) <- None;
  pfile.len <- pfile.len -1

let rm_config node peer = (*Retire peer de la config de node*)
  try
    let pfile = node.config in
    let j = ref 0 in
    while !j < pfile.len do
      (*Printf.printf "node %d -> " (id pfile.tab.(!j));*)
      match pfile.tab.(!j) with
      | Some c when c.n.id = peer.id -> (
        (*print_string "Removing it. ";*)
        remove_nth pfile !j;
        raise Ok
      )
      | _ -> j := !j + 1
    done;
    failwith "Rien rm dans pfile"
  with Ok -> ()(*print_string "Finished rm_pfile. "*)



let best_config node = 
  if node.config.len > Array.length node.config.tab || node.config.len <= 0 then failwith "Incohérence"
  else
  match node.config.tab.(node.config.len -1) with 
  |  None -> failwith "Pas de best"
  |  Some n -> n


let affiche_reseau reseau = (*Prends un node option array*)
  for i = 0 to reseau.len_noeuds -1 do
    match reseau.noeuds.(i) with 
    | None -> print_string "None, "
    | Some node -> 
      match node.couplage.tab.(if node.couplage.len -1 >= 0 then node.couplage.len -1 else 0) with 
      |  None -> Printf.printf " Id: %d, len_couplage: %d, len_config: %d, pref_in_matc_h: None\n" node.id node.couplage.len node.config.len
      |  Some best ->
      Printf.printf "Id: %d, len_couplage: %d, len_config: %d, pref_in_match: %d\n" node.id node.couplage.len node.config.len best.n.id
  done
  

let affiche_tab arr = 
  for i = 0 to Array.length arr -1 do 
    match arr.(i) with 
    |  None -> print_string "None, "
    |  Some n -> Printf.printf "Some %d, " n.id
  done; 
  print_newline ()


let nullify_noeuds reseau node = (*On enlève node du reseau.noeuds et on met le dernier élément à la place comme ça tout reste rangé comme il faut*)
  if node.ind_noeuds >= reseau.len_noeuds || node.ind_noeuds < 0 then 
    failwith "Essai de retirer du graphe un noeud qui n'y es déjà plus, ou en tout cas les indices ne vont pas"  
  else begin 
    assert (reseau.noeuds.(reseau.len_noeuds) =  None);
    let node_last_op = reseau.noeuds.(reseau.len_noeuds -1) in
    match node_last_op with 
    |  None -> failwith "Bizarre, le dernier element de reseau.noeuds est null"
    |  Some node_last -> (
      reseau.noeuds.(node.ind_noeuds) <- Some node_last;
      reseau.noeuds.(reseau.len_noeuds -1) <- None;
      reseau.len_noeuds <- reseau.len_noeuds -1;

      node_last.ind_noeuds <- node.ind_noeuds;
      node.ind_noeuds <- -1);
  end


let nullify_unloving reseau node = (*On enlève node du reseau.unloving et on met le dernier élément node_last à la place comme ça tout reste rangé comme il faut*)
  if node.ind_unloving >= reseau.len_unloving || node.ind_unloving < 0 then  
    failwith "Essai de retirer un noeud de unloving mais son id ne correspond pas"
  else begin
    let node_last_op = reseau.unloving.(reseau.len_unloving -1) in
    assert(reseau.unloving.(reseau.len_unloving) = None);
    match node_last_op with 
    |  None -> failwith "Bizarre, le dernier element de reseau.loving est null"
    |  Some node_last -> (
      reseau.unloving.(node.ind_unloving) <- Some node_last;
      reseau.unloving.(reseau.len_unloving-1) <- None;
      reseau.len_unloving <- reseau.len_unloving -1;

      node_last.ind_unloving <- node.ind_unloving;
      node.ind_unloving <- -1)                        (*SI JAMAIS TU AS UNE ERREUR DE MEMOIRE C'EST POSSIBLEMENT A CAUSE DE CA*)
  end




let reseau_init () =
  let noeuds = Array.make max_noeuds None in
  let unloving = Array.make max_noeuds None in
  let r = {
    noeuds = noeuds;
    len_noeuds = 0;
    unloving = unloving;
    len_unloving = 0;
    data = {
      cycles_coupe = 0;
      noeuds_del = 0;};
  } in 
  r



(*    -----     Calcul du couplage    ------    *)


let mem_lover node peer = 
  try
    for i = 1 to node.num_loving do 
      if peer.id = id node.couplage.tab.(node.couplage.len -i) then raise Ok 
    done;
    false
  with Ok -> true


let try_change pfile n b peer = (*Essaie de modifier la pfile, lève Ok si elle réussit*)
  match pfile.tab.(n) with
    | Some p when p.n.id = peer.id -> (
      pfile.tab.(n) <- Some {n = p.n; e = b; marque = p.marque};
      raise Ok)
    | _ -> ()


let set_flag_in_pfile pfile peer b i =        (*Possiblement faire une amélioration algorithmique la dessus, c'est lourd
                                L'idée c'est que soit on connait l'indice à changer auquel cas on met un Some dans le i
                                soit on connait pas, auquel cas on parcourt tous les nodes de la pfile en cherchant peer
                                On échoue si peer n'est pas dans pfile*)
  match i with 
  |  None -> (
    try
      for j = 0 to pfile.len - 1 do
        try_change pfile j b peer
      done;
      failwith "No flag set"
    with Ok -> ()
  )
  |  Some n -> 
    try_change pfile n b peer



let flag ch = 
  match ch with 
  |  None -> Unmatched (*Completement arbitraire*)
  |  Some c -> c.e


exception Found of int


let marquen node peer def = (*donne la marque de node depuis chez peer. Renvoie def si 
    peer n'y apparait pas. C'est ptt error prone*)
  try
    for i = 0 to peer.config.len -1 do 
      match peer.config.tab.(i) with 
      |  None -> raise (Found def)
      |  Some n when n.n.id = node.id -> 
        raise (Found n.marque)
      |  _ -> ()
    done;
    def
  with Found m -> m



(*C'est vraiment ridicule cette ENORME chaîne de and*)


let rec remove_from_couplages reseau node e = (*Retire node de tous les couplages imparfaits
          e est un indicateur pour dire si on doit retirer node même s'il est loving ou pas*)
  for i = 0 to reseau.len_unloving -1 do
    let p = nth_unloving reseau i in
    let pfile = p.couplage in 
      (*Printf.printf "Removing from %d's matching: " p.id;*)
        let i = ref 0 in
        while !i < pfile.len - 1 do
          (*Printf.printf "node %d -> " (id pfile.tab.(!i));*)
          if id pfile.tab.(!i) = node.id && (flag pfile.tab.(!i) != Loving || not e) then (
              (*print_string "Removing ... ";*)
              remove_nth pfile !i; (*Pas de +1 vu qu'on a tout décalé*)
              if flag pfile.tab.(!i) = Loving then 
                p.num_loving <- p.num_loving -1;
              set_flag_in_pfile p.config node Unmatched None;
          )
          else 
            incr i 
        done;

      (*check_loving p reseau;*)                (*On vérifie que le retrait du node n'engendre pas un lover après num_loving*)

  done

and traitement_loving node peer reseau = 
  (*Printf.printf "LOVING between %d and %d - " node.id peer.id;(*C'est une loving pair*)*)
  node.num_loving <- node.num_loving +1;
  peer.num_loving <- peer.num_loving +1;

  set_flag_in_pfile node.couplage peer Loving None; (*Potentiellement aller chercher le bon indice*)
  set_flag_in_pfile peer.couplage node Loving None;

  if node.num_loving = !b then begin (*Le node est complètement couplé qu'avec des paires incassables: on le retire de unloving*)
    (*Printf.printf "removing node - ";*)
    nullify_unloving reseau node; 
    remove_from_couplages reseau node true;
    (*Printf.printf "Nullified \n "*)
  end;
  if peer.num_loving = !b then begin
    (*Printf.printf "removing peer - ";*)
    nullify_unloving reseau peer;
    remove_from_couplages reseau peer true;
    (*Printf.printf "Nullified \n"*)
  end;
  rm_config node peer;
  rm_config peer node;


and check_loving node peer reseau vus = 
  if node.config.len > 0 then begin
    let best = best_config node in
    if best.n.id = peer.id && (best_config peer).n.id = node.id then (              (*Erreur possible ici quant au best_config peer*)
      (*Printf.printf "Adding: %d loving with %d ... " node.id peer.id;*)
      traitement_loving peer node reseau)
    else
      launch_initiative reseau peer vus
  end
  else failwith "node avec config vide" 




and couplement node peer reseau mn mp vus = (*On a trouvé une paire devant être couplée: 
  On ajoute la paire au couplage et 
    si la paire est loving on retire de chaque config le pair correspondant et 
    on les enlève du unloving du réseau et on réduit le unloving du reseau si ils sont complètement couplés avec des loving*)

  pfile_insere node.couplage peer Unloving mn;
  pfile_insere peer.couplage node Unloving mp;

  set_flag_in_pfile node.config peer Unloving None;
  set_flag_in_pfile peer.config node Unloving None;

  match peer.config.tab.(peer.config.len -1) with 
  |  None -> (
    print_string "\nLe peer choisi n'a pas de config\n"; 
    exit(1))
  |  Some c when c.n.id != node.id -> (
    (*print_string "Pas loving - "; *)
    launch_initiative reseau peer vus)
  |  Some c ->
    traitement_loving node peer reseau
  
  (*;

  
  print_string "Removing two new coupled from each other's config\nNode:";
  rm_pfile node.config peer peer;
  print_string "\nPeer:";
  rm_pfile peer.config node node*)
  


and casse_match node peer = (*Retire peer du couplage de node*)
  try
    for i = 0 to node.couplage.len -1 - node.num_loving do 
      let matc = node.couplage.tab.(i) in 
      if id matc = peer.id then (
        remove_nth node.couplage i;
        set_flag_in_pfile node.config peer Unmatched None;
        raise Ok)
    done;
    failwith "rien cassé ... " (*Ca veut dire qu'on a ajouté wn à config sans le retirer de couplage -> ca craint*)
  with Ok -> ()


and marque ch = 
  match ch with 
  |  None -> -1
  |  Some c -> c.marque


and proposal node peer reseau mn mp vus = (*Immonde, à nettoyer*)
  let wno = worst node in 
  let wpo = worst peer in
  if node.couplage.len = !b && peer.couplage.len = !b then 
    match wno, wpo with 
    |  Some wn, Some wp when mp >= wp.marque && mn >= wn.marque ->( 
      (*On s'assure que faire un changement garantit de trouver un meilleur état*)
      (*On retire les worst mate de peer et node pour les remplacer avec un autre mate de marque str sup*)
        (*print_string "changement du couplage - ";*)

        (*pfile_insere node.config wn false;
        pfile_insere peer.config wp false;*)


        casse_match wn.n node;
        casse_match wp.n peer;

        remove_nth peer.couplage 0;
        set_flag_in_pfile peer.config wp.n Unmatched None;
        remove_nth node.couplage 0;
        set_flag_in_pfile node.config wn.n Unmatched None;

        couplement node peer reseau mn mp vus)

    |  Some n, None | None, Some n -> failwith "Broken :/"
    | None, None -> failwith "Broken :/"
    |  _ ->          (*Il n'est pas intéressant de faire de changement*)
        (*Printf.printf "Pas de changement1 ... ";*)
        launch_initiative reseau peer vus (*!!! On relance l'initiatvie depuis peer*)
        (*affiche_node_array node.couplage;
        affiche_node_array peer.couplage;
        affiche_node_array node.config;
        affiche_node_array peer.config
      end*)

  else if node.couplage.len = !b && marque wno <= mn then
    match wno with 
    |  Some wn ->( 
      (*Printf.printf "Ajout chez peer, changement chez node - ";*)
      (* pfile_insere node.config wn false;*)
      assert(node.num_loving < !b);
      casse_match wn.n node;      (* On enlève le worst du couplage de node pour avoir la place pour peer (qui est mieux)*)

      remove_nth node.couplage 0;
      set_flag_in_pfile node.config wn.n Unmatched None;

      couplement node peer reseau mn mp vus)

    |  None -> failwith "Broken :/"

  else if peer.couplage.len = !b && marque wpo <= mp then 
    match wpo with 
    |  Some wp ->( 
      (*Printf.printf "Ajout chez node, changement chez peer - ";*)
      (*pfile_insere node.config wn false;*)
      assert(peer.num_loving < !b);
      casse_match wp.n peer;      (* On enlève le worst du couplage de node pour avoir la place pour peer (qui est mieux)*)

      remove_nth peer.couplage 0;
      set_flag_in_pfile peer.config wp.n Unmatched None;

      couplement node peer reseau mn mp vus)

    |  None -> failwith "Broken :/"

  else if peer.couplage.len != !b && node.couplage.len != !b then (
    (*print_string "Ajout aux deux ... ";*)
    couplement node peer reseau mn mp vus
  )
  else (
    (*Printf.printf "Pas de changement2; nlovers = %d, plovers = %d... " node.num_loving peer.num_loving;*)
    launch_initiative reseau peer vus) (* 
    begin
        Printf.printf "Pas de changement: num_loving du node: %d - num_loving du peer: %d\nCouplage_node / Couplage_peer / Config_node / Config_peer:\n" node.num_loving peer.num_loving;
        affiche_node_array node.couplage;
        affiche_node_array peer.couplage;
        affiche_node_array node.config;
        affiche_node_array peer.config
      end*)

and launch_initiative reseau node vus =
    if node.config.len > 0 then begin
      match node.config.tab.(node.config.len -1) with
      |  None -> failwith "Erreur: le best mate du node est à None"
      |  Some best -> begin 
        assert(best.e <> Loving);
        match Hashtbl.find_opt vus best.n.id with 
        |  Some p -> begin
          (*Printf.printf "Cycle !! On retire %d depuis chez %d" best.n.id node.id;*)
              (*Passage critique; à voir si y a vrmnt besoin de retirer à la fois node et peer.
                Pour l'instant c'est assez dommage pour le couplage*)

          rm_config node best.n;
          rm_config best.n node;

          let e = ref (true, true) in
          try
            casse_match node best.n;
            exit(1);
          with Failure(_) -> e := false, true;
          if fst !e then 
            set_flag_in_pfile node.couplage best.n Unmatched None;

          try
            casse_match best.n node;
            exit(1);
          with Failure(_) -> e := false, false;
          if snd !e then 
            set_flag_in_pfile best.n.couplage node Unmatched None;

          (*Somehow toute cette partie ne sert à rien*)
          reseau.data <- {cycles_coupe = reseau.data.cycles_coupe +1; noeuds_del = reseau.data.noeuds_del} 
        end

        |  None -> begin
          Hashtbl.add vus best.n.id node.id;
          if best.n.ind_unloving = -1 || best.n.ind_noeuds = -1 then ((* On retire de la config et on repioche. Le retrait d'un node cause au plus len_config repiochages -> ok complexité
                                                                          Ceci arrive quand un best couplé apparaît dans la config de node *)
              (*Printf.printf "Le peer %d a été retiré ...  " best.n.id;*)
              let _ = pfile_defile node.config in 
              ()
          )
          else if best.e != Unmatched then (
              (*Printf.printf "Le peer %d a déjà été ajouté - on test_loving ...  " best.n.id;*)
              check_loving node best.n reseau vus
          )
          else (                                     (* On voit s'il est intéressant de coupler node et best -> c'est intéressant pour node, l'est-ce pour best?*)
            (*Printf.printf "Lancement proposal depuis %d avec %d (%d), ... " node.id best.n.id best.marque;*)
            let mp = marquen node best.n best.marque in
            proposal node best.n reseau best.marque mp vus
          )
        end
      end
    end

    else begin
      (*Printf.printf "Absurdité: node %d a une config de taille %d\n" node.id node.config.len;*)
      (*affiche_reseau reseau;*)
      (*On retire le noeud faulty au lieu de tout abandonner*)
      node_del reseau node;
      reseau.data <- {cycles_coupe = reseau.data.cycles_coupe; noeuds_del = reseau.data.noeuds_del +1};
    end



and node_del reseau node = 
  (*Retrait de node de reseau*)
  remove_from_couplages reseau node false;
  for i = 0 to reseau.len_noeuds -1 do
    match reseau.noeuds.(i) with 
    |  None -> failwith "Nan"
    |  Some n -> 
      try rm_config n node with Failure(s) -> (); (*Ptt un peu bizarre*)
  done;
  nullify_noeuds reseau node;
  nullify_unloving reseau node


let rec choix_best reseau = 
  let i = randint reseau.len_unloving in 
  match reseau.unloving.(i) with
    |  None -> failwith "Soucis, il y a un None au milieu du unloving"
    |  Some node -> 
      (*Printf.printf "Choisi %d parmi %d ... " node.id reseau.len_noeuds;*)
      node 



let marque node = 
  match node with 
  |  None -> -1
  |  Some n -> n.marque



(*let ajout_de_nodes reseau node = 
    for i = 0 to min (reseau.len_unloving -1) max_config do           (*Le choix n'est pas trop aléatoire, idéalement faire un connection_init*)
        match reseau.unloving.(i) with 
        |  None -> failwith "Un vide au milieu du unloving"
        |  Some n when n.id != node.id -> (
          Printf.printf "Inserting %d ... " n.id;
          pfile_insere node.config n false ;
          pfile_insere n.config node false)
        |  _ -> ()
    done;
    Printf.printf "New config de taille %d ... " node.config.len*)


let mem_couplage node peer = 
  try
    for i = 0 to node.couplage.len -1 do 
      if peer.id = id node.couplage.tab.(i) then raise Ok 
    done;
    false
  with Ok -> true





let rec initiative_best reseau vus = (*Réalise une initiative via stratégie best mate*)
  let node = choix_best reseau in
  assert(not (has_before node.config node 0));
  Hashtbl.add vus node.id node.id;
  launch_initiative reseau node vus



let protocol reseau = 
  (*print_string "Début protocol\n";*)
  num.num <- 0;
  let c = ref 0 in
  try
    while reseau.len_unloving > 0 do 
      (*Printf.printf "Lancement à len_unloving = %d ... " reseau.len_unloving; *)
      let vus = Hashtbl.create 42 in
      initiative_best reseau vus;
      incr c;
      (*print_string "Initiative finie\n\n"*)
    done
    (*Printf.printf "Convergence réussie\nCompteur: %d étapes" !c *)
  with Ok -> Printf.printf "Convergence aborted, ratio de réussite à %d/%d" (reseau.len_noeuds - reseau.len_unloving) reseau.len_noeuds 
  

let protocol_vtest reseau = 
  print_string "Début protocol\n";
  for i = 0 to 100 do 
    Printf.printf "Lancement à len_unloving = %d ..." reseau.len_unloving; 
    let vus = Hashtbl.create 42 in
    initiative_best reseau vus;
    print_string "Initiative finie\n\n"
  done;
  print_string "Convergence réussie"
  


(*    ----------  MISC functions  -------     *)




let len_reseau reseau = 
  reseau.len_noeuds


let config node = 
  node.config



let couplage node = 
  node.couplage


let lr node = 
  node.ind_noeuds


let noeud c = 
  c.n


let get_b () = 
  !b

let set_b new_b = 
  b := new_b


let cycles_coupe r = 
  r.data.cycles_coupe


let noeuds_del r = 
  r.data.noeuds_del


exception Found_node of node



let exists_node r id = (*Renvoie Some node si node est dans le réseau et node.id = id
                                 None sinon*)
  try
    for i = 0 to r.len_noeuds -1 do 
      match r.noeuds.(i) with 
      |  Some node when node.id = id -> raise (Found_node node) 
      |  _ -> ()
    done;
    None 
  with Found_node node -> Some node


let rank_in pfile id = (*Renvoie le rang de id +1 dans pfile si id y est, 0 sinon*)
  try
    for i = 0 to pfile.len -1 do 
      match pfile.tab.(i) with 
      |  Some node when node.n.id = id -> raise (Found i) 
      |  _ -> ()
    done;
    0
  with Found i -> i


let f = float_of_int


let stabilite r0 r = (*Quantifie une mesure s de la qualité du couplage
                      obtenu dans r comparé aux config de r0
                      0 <= s <= 1*)
  let sat = ref 0. in 
  for i = 0 to r0.len_noeuds -1 do 
    match r0.noeuds.(i) with 
    | None -> failwith "Pas possible"
    | Some node0 -> 
      match exists_node r node0.id with 
      |  None -> ()
      |  Some node -> 
        let static = (f node.couplage.len) /. (f !b) in 
        sat := !sat +. static;
        for i = 0 to node.couplage.len -1 do
          match node.couplage.tab.(i) with 
          |  None -> ()
          |  Some coupled -> 
            let esp = f ((rank_in node0.config coupled.n.id) - (node.couplage.len -i)) in 
            let added = esp /. ((f node0.config.len) *. (f !b)) in 
            assert(added <= 1.);
            sat := !sat -. added
        done
  done; 
  let s = !sat /. (f r0.len_noeuds) in 
  s


(*      Quelques fonctions pour copier un réseau      *)

let pfile_init () = 
  {tab = Array.make max_noeuds None;
  len = 0;}


let node_init_without_pfile id ind_n ind_u n_lov = 
  {id = id;
  ind_noeuds = ind_n;
  ind_unloving = ind_u;
  config = pfile_init ();
  couplage = pfile_init ();
  num_loving = n_lov;}



let copy_pfiles pf1 pf2 r1 r2= 
  for i = 0 to pf1.len -1 do 
    match pf2.tab.(i) with 
    |  None -> 
      pf1.tab.(i) <- None
    |  Some ch -> 
      pf1.tab.(i) <- Some {n = nth_noeuds r1 ch.n.ind_noeuds; e = ch.e; marque = ch.marque} 
  done
  

let reseau_copy r0 = 
  let r = reseau_init () in 
  r.len_unloving <- r0.len_unloving;
  r.len_noeuds <- r0.len_noeuds;

  (*copie de r0.noeuds*)
  for i = 0 to r.len_noeuds -1 do 
    match r0.noeuds.(i) with 
    |  None -> 
      r.noeuds.(i) <- None
    |  Some node ->
      r.noeuds.(i) <- Some (node_init_without_pfile node.id node.ind_noeuds node.ind_unloving node.num_loving)
  done;

  (*copie de r0.unloving*)
  for i = 0 to r.len_unloving -1 do 
    match r0.unloving.(i) with 
    |  None -> 
      r.unloving.(i) <- None
    |  Some node ->
      match r.noeuds.(node.ind_noeuds) with 
      |  None -> failwith "Pas bon"
      |  Some peer when peer.id <> node.id -> failwith "Pas bon"
      |  Some peer -> (*peer est une copie de node normalement*) 
        r.unloving.(i) <- Some peer
  done;
  

  (*copie des configs et couplages*)
  for i = 0 to r.len_noeuds -1 do 
    match r.noeuds.(i), r0.noeuds.(i) with 
    |  None, None -> ()
    |  None, Some n | Some n, None -> failwith "Nannnn"
    |  Some n1, Some n2 -> 
      n1.config.len <- n2.config.len;
      n1.couplage.len <- n2.couplage.len;
      copy_pfiles n1.couplage n2.couplage r r0;
      copy_pfiles n1.config n2.config r r0
  done;

  r