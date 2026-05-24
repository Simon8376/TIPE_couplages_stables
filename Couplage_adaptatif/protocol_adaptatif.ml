


let max_noeuds = 1000;; (*Nombre max d'utilisateurs*)
let max_marque = 100000;; (*La qualité d'une connexion est évaluée entre 0 et 1000*)
let max_config = 1000;; (*Degré maximal d'un sommet dans le graphe d'acceptance. ATTENTION, CHANGER CA POURRAIT CASSER LE CODE *)
let b = ref 1;; (*Nombre d'arêtes du couplage incidentes à chaque sommet: on cherche un b-couplage*)

Random.init (int_of_float (Sys.time () *. 100000000.));


type trileen = Unmatched | Unloving | Loving (*Différents états d'un noeud au sens d'une configuration / d'un couplage*)


type node = {
  id: int;
  mutable ind_noeuds : int; (*indice du node dans le tableau reseau.noeuds *)
  mutable ind_unloving : int; (*indice du node dans reseau.unloving*)
  config : pfile; (* Tableau de noeuds rangé par marque décroissante des nodes avec lesquels le node peut échanger des données.
    C'est la liste d'adjacence de node dans le graphe d'acceptance.
    Les cases non utilisées sont à la fin du tableau et à None*)
  couplage : pfile; (*Tableau de taille b ordonné par marque décroissante des nodes intervenant dans le couplage
                    Le deuxième tableau de la pfile est inutilisé...*)
  mutable num_loving : int; (*Nombre (majoré par b et par couplage.len) de peers au sein de couplage qui sont loving*)
}
and pfile = { (*Tableau trié selon la marque décroissante. Les cases non utilisées sont à None
                Il s'agit (pour tab et len) de deux tableaux dans un tableau, où le premier est une
                version de travail et le deuxième une mémoire globale dont on ne retire pas trop d'informations*)
  mutable tab : champ option array array;
  mutable len : int array;
}
and champ = { (*Structure modélisant une arête depuis un noeud*)
  n : node;
  etat : trileen; (*indique si node est dans le couplage ou pas, relevant seulement pour config*)
  marque : int;
  tmps_debut : int; (*Indique l'étape de création du lien. Une étape
                    c'est un ajout de noeud (i entre 0 et 150). Ce n'est 
                    intéressant que dans un champ de couplage*)
}

type reseau = {
  mutable noeuds : node option array; (*tableau des nodes dans le réseau. Le node d'indice node.id est à l'indice node.id*)
  mutable len_noeuds : int; (*Taille de noeuds*)
  mutable len_unloving : int; (*Taille de unloving. Le protocol s'arrête quand ça atteint 0*)
  mutable unloving : node option array; (*tableau des noeuds dont le couplage n'est pas composé que de peers loving. Idem niveau node.id *)
  mutable num: int; (*C'est le max des indices des noeuds du réseau +1. Ca permet d'ajouter des noeuds*)
  mutable step : int; (*Etape actuelle (ajout de node => différente étape)*)
  mutable tmps_vie : int list; (*Liste de temps de vie d'arêtes qui ont été supprimées*)
}

exception Ok

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

let affiche_node_array pfile e = 
  try
    for i = 0 to Array.length pfile.tab.(e) -1 do 
      match pfile.tab.(e).(i) with 
      |  None -> (
        Printf.printf "None\n";
        raise Ok
        )
      |  Some c -> 
          match c.n.couplage.tab.(e).(if c.n.couplage.len.(e) -1 >= 0 then c.n.couplage.len.(e) -1 else 0) with 
          |  None ->  
            Printf.printf "Some %d with marque %d is alone\n" c.n.id c.marque
          |  Some best when c.n.num_loving >= 1 -> 
            Printf.printf "Some %d (%s) with marque %d is loving with %d\n" c.n.id (tril_to_string c.etat) c.marque best.n.id
          |  Some best -> 
            Printf.printf "Some %d (%s) with marque %d is coupled with %d\n" c.n.id (tril_to_string c.etat) c.marque  best.n.id
    done;
  with Ok -> ();
  print_newline ()




(*  ----- Implémentation de la file de priorité - NAN -----   *)

let switch pfile e i j = 
  if i < 0 || j < 0 then 
    failwith "Erreur, switch sur des indices interdits"
  else begin
    match pfile.tab.(e).(i), pfile.tab.(e).(j) with 
    |  ni, nj ->
      pfile.tab.(e).(i) <- nj;
      pfile.tab.(e).(j) <- ni
  end



let has_before pfile e node n = 
  try
    for i = 0 to pfile.len.(e) -1 - n do 
      match pfile.tab.(e).(i) with 
      |  Some c -> 
      if c.n.id = node.id then 
        raise Ok
      |  None -> ()
    done;
    false
  with Ok -> true
    




let pfile_insere r pfile e node etat marque =
  let i = ref (pfile.len.(e) -1) in
  (if pfile.tab.(e).(!i+1) <> None || (!i >= 0 && pfile.tab.(e).(!i) = None) then
    (
    (*affiche_node_array pfile e;*)
    failwith "Nein")
   );
  (try 
    while !i >= 0 do 
      match pfile.tab.(e).(!i) with 
      |  None -> ((*affiche_node_array pfile e;*) failwith "Un none au milieu")
      |  Some c when c.n.id = node.id -> failwith "node est déjà dans la pfile"
      |  Some c when c.marque > marque -> (
        assert(!i+1 <= pfile.len.(e));
        switch pfile e !i (!i+1);
        i := !i-1)
      |  Some c -> 
        raise Ok
    done
  with Ok -> ());
  pfile.tab.(e).(!i+1) <- Some {n = node; etat = etat; marque = marque; tmps_debut = r.step};
  pfile.len.(e) <- pfile.len.(e) +1



let pfile_defile pfile e = (*Renvoie et retire le plus grand élément de la pfile*)
  if pfile.len.(e) > Array.length pfile.tab.(e) then failwith "Erreur dans defile, la len est plus grande que la taille de l'array"
  else if pfile.len.(e) = 0 then failwith "Défilement sur une pfile vide"
  else
    let n = pfile.tab.(e).(pfile.len.(e) -1) in 
    pfile.tab.(e).(pfile.len.(e) -1) <- None;
    pfile.len.(e) <- pfile.len.(e) -1;
    n



let est_trie pfile e = (*Vérifie si pfile est triée. La réordonne si c'est 
                        pas le cas*)
  try
    for i = 0 to pfile.len.(e) -2 do 
      match pfile.tab.(e).(i), pfile.tab.(e).(i+1) with 
      |  None, _ | _, None -> (
        (*affiche_node_array pfile e;*)
        failwith "None au milieu")
      |  Some c1, Some c2 ->
        if c1.marque > c2.marque then (
          (*Printf.printf "failed between %d and %d out of %d" i (i+1) (pfile.len.(e)-1);*)
          raise Not_found)
    done;
    true
  with Not_found -> false



(*MISC*)

let worst node = 
  node.couplage.tab.(0).(0)


let empty_pfile () = 
  {
    len = [|0; 0|];
    tab = [|Array.make max_config None; Array.make max_config None|];
  }

let rec affiche_liste_int liste = 
  match liste with 
  |  [] -> 
    Printf.printf "Vide\n"
  |  [h] -> 
    Printf.printf "%d\n" h
  |  h :: t -> 
    Printf.printf "%d, " h;
    affiche_liste_int t


let randint borne = 
  if borne <= 0 then
    0 
  else
    Random.int borne



(*  -------   Initialisation d'un nouveau node    --------   *)
(*      ---------- et Génération de graphes --------       *)
(*Quelques fonctions permettant de produire des graphes aléatoires
selon les modèles d'erdos et renyi ou encore selon la loi de puissance.
- Erdos-Renyi: chaque arêtes est présente avec probabilité p, puis avec marque aléatoire
- Loi de puissance: chaque sommet a probabilité k^(-y) d'avoir k sommets
où 2 < k < 3 typiquement

Notons que l'algorithme de couplage nécessite des graphes complets. Les arêtes
non présentes selon les modèles ont pour marque 0*)

(*Cf la partie "Initialisation d'un node" pour un génération de marques
totalement aléatoires*)



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
  |  None -> failwith "y a pas"
  |  Some n -> n



let connections_init p reseau = (*Génère une liste de préférence selon erdos-renyi*)
  let pf ={
    tab = [|Array.make max_config None; Array.make max_config None|];
    len = [|0; 0|];
  } in

  for i = 0 to reseau.len_noeuds -1 do 
    match reseau.noeuds.(i) with 
    |  None -> failwith "None au milieu des noeuds"
    |  Some node -> (
      let max = (2 lsl 29) -1 in 
      let n = randint max in 
      let m = 
        if float_of_int n < p *. (float_of_int max) then 
          (randint max) mod max_marque
        else
          0
      in
      pfile_insere reseau pf 1 node Unmatched m;
      pfile_insere reseau pf 0 node Unmatched m)
  done;
  assert(pf.len.(0) = reseau.len_noeuds);
  
  pf 


let corrige_len pfile e = 
  let new_l = ref 0 in 
  while pfile.tab.(e).(!new_l) <> None do 
    incr new_l 
  done;
  pfile.len.(e) <- !new_l



let node_init p reseau = (*Prend en argument une fonction de construction de configuration
                      et un reseau et renvoie un noeud qui est ajouté au réseau
    node_init se contente d'ajouter un noeud aux configurations et ne 
    se préoccupe pas de vérifier si les couples dans les couplages sont 
    toujours loving*)
  let pf = connections_init p reseau in
  let node = {
    id = reseau.num;
    ind_noeuds = reseau.len_noeuds;
    ind_unloving = reseau.len_unloving;
    config = pf;
    couplage = {
      tab = [|Array.make max_config None; Array.make max_config None|];
      len = [|0; 0|];
    };
    num_loving = 0;
  } 
  in 
  reseau.num <- reseau.num +1;
  
  (*il faut ensuite ajouter n aux ensemble de configuration de tous les autres pour symétrie*)  
  assert(node.config.tab.(0).(node.config.len.(0)) = None);
  for e = 0 to 1 do
    for i = 0 to node.config.len.(e) -1 do 
      match node.config.tab.(e).(i) with
      |  None -> failwith "Erreur dans le len de la config qui ne correspond pas aux Some / None"
      |  Some c when c.n.id = node.id -> failwith "Gunk"
      |  Some c -> (
        try
          pfile_insere reseau c.n.config e node Unmatched c.marque
        with Failure(s) -> (
          if s = "Nein" then ( (*C'est artificiel*)
            (*let l = c.n.config.len.(e) in*)
            corrige_len c.n.config e;
            (*Printf.printf "Corrected len of %d from %d to %d\n" c.n.id l c.n.config.len.(e);*)
            pfile_insere reseau c.n.config e node Unmatched c.marque)
          else 
            failwith s))
    done
  done;
      

  (*Et ajouter n à la liste des sommets*)
  reseau.len_noeuds <- reseau.len_noeuds +1;
  reseau.noeuds.(node.ind_noeuds) <- Some node;

  reseau.len_unloving <- reseau.len_unloving +1;
  reseau.unloving.(node.ind_unloving) <- Some node;

  node




let nth_noeuds reseau i = 
  match reseau.noeuds.(i) with 
  |  None -> failwith "y a pas"
  |  Some n -> n


let remove_nth r pfile e j =
  if pfile.len.(e) = 0 then failwith "Nothing to remove"
  else
    let start = 
      match pfile.tab.(e).(j) with 
      |  None -> failwith "Rien"
      |  Some node -> node.tmps_debut 
    in
    for i = j to pfile.len.(e) -2 do 
      switch pfile e i (i+1)
    done;
    pfile.tab.(e).(pfile.len.(e) -1) <- None;
    pfile.len.(e) <- pfile.len.(e) -1;
    r.step - start


let rm_config r e node peer = (*Retire peer de la config de node*)
  try
    let pfile = node.config in
    for j = pfile.len.(e) -1 downto 0 do
      match pfile.tab.(e).(j) with
      | Some c when c.n.id = peer.id -> (
        let _ = remove_nth r pfile e j in
        raise Ok
      )
      | _ -> ()
    done;
    failwith "Rien rm dans pfile"
  with Ok -> ()(*print_string "Finished rm_pfile. "*)



let best_config e node = 
  match node.config.tab.(e).(node.config.len.(e) -1) with 
  |  None -> failwith "Pas de best"
  |  Some n -> n




let nullify_noeuds reseau node = (*On enlève node du reseau.noeuds et on met le dernier élément à la place comme ça tout reste rangé comme il faut*)
  if node.ind_noeuds >= reseau.len_noeuds || node.ind_noeuds < 0 then 
    failwith "Essai de retirer du graphe un noeud qui n'y es déjà plus, ou en tout cas les indices ne vont pas"  
  else begin 
    assert (reseau.noeuds.(reseau.len_noeuds) =  None);
    let node2_op = reseau.noeuds.(reseau.len_noeuds-1) in
    match node2_op with 
    |  None -> failwith "Bizarre, le dernier element de reseau.noeuds est null"
    |  Some node2 -> (
      reseau.noeuds.(node.ind_noeuds) <- Some node2;
      reseau.noeuds.(reseau.len_noeuds -1) <- None;
      reseau.len_noeuds <- reseau.len_noeuds -1;

      node2.ind_noeuds <- node.ind_noeuds;
      node.ind_noeuds <- -1);
  end


let affiche_tab arr = 
  try
    for i = 0 to Array.length arr -1 do 
      match arr.(i) with 
      |  None -> (print_string "None, "; raise Ok)
      |  Some n -> 
        Printf.printf "Some %d, " n.id
    done
  with Ok -> 
    print_newline ()


let affiche_reseau reseau = (*Prends un node option array*)
  for i = 0 to reseau.len_noeuds -1 do
    match reseau.noeuds.(i) with 
    | None -> print_string "None, "
    | Some node -> 
      let n = node.couplage.len.(0) in 
      if n = 0 then 
        Printf.printf "Id: %d, len_couplage: %d, len_config_t: %d, len_config_g: %d, loved: %d, pref: None\n" node.id node.couplage.len.(0) node.config.len.(0) node.config.len.(1) node.num_loving
      else
        match node.couplage.tab.(0).(n-1) with
        |  None -> failwith ""
        |  Some best ->
          Printf.printf "Id: %d, len_couplage: %d, len_config_t: %d, len_config_g: %d, loved: %d, pref: %d\n" node.id node.couplage.len.(0) node.config.len.(0) node.config.len.(1) node.num_loving best.n.id
  done;
  print_string "Unloving: \n";
  affiche_tab reseau.unloving;
  print_string "Noeuds: \n";
  affiche_tab reseau.noeuds
  


let nullify_unloving reseau node = (*On enlève node du reseau.unloving et on met le dernier élément node_last à la place comme ça tout reste rangé comme il faut*)
  if node.ind_unloving >= reseau.len_unloving || node.ind_unloving < 0 then  
    failwith "Essai de retirer un noeud de unloving mais son id ne correspond pas"
  else begin
    let node_last_op = reseau.unloving.(reseau.len_unloving-1) in
    assert(reseau.unloving.(reseau.len_unloving) = None);
    match node_last_op with 
    |  None -> failwith "Bizarre, le dernier element de reseau.loving est null"
    |  Some node_last -> (
      reseau.unloving.(node.ind_unloving) <- Some node_last;
      reseau.unloving.(reseau.len_unloving-1) <- None;
      reseau.len_unloving <- reseau.len_unloving -1;

      node_last.ind_unloving <- node.ind_unloving;
      node.ind_unloving <- -1);                          (*SI JAMAIS TU AS UNE ERREUR DE MEMOIRE C'EST POSSIBLEMENT A CAUSE DE CA*)
  end


let reseau_init () =
  let noeuds = Array.make max_noeuds None in
  let unloving = Array.make max_noeuds None in
  let r = {
    noeuds = noeuds;
    len_noeuds = 0;
    unloving = unloving;
    len_unloving = 0;
    num = 1;
    step = 0;
    tmps_vie = [];
  } in 
  r



(*    -----     Calcul du couplage    ------    *)


let mem_lover node peer = 
  try
    for i = 1 to node.num_loving do 
      if peer.id = id node.couplage.tab.(0).(node.couplage.len.(0) -i) then raise Ok 
    done;
    false
  with Ok -> true


let try_change pfile e i b peer = (*On ne change que le flag*)
  match pfile.tab.(e).(i) with
    | Some p when p.n.id = peer.id -> (
      pfile.tab.(e).(i) <- Some {n = p.n; etat = b; marque = p.marque; tmps_debut = p.tmps_debut};
  
      (*Printf.printf "Flag changed of %d" peer.id;*)
      raise Ok)
    | _ -> ()


let set_flag_in_pfile pfile e peer b i =        (*Possiblement faire une amélioration algorithmique la dessus, c'est lourd*)
  match i with 
  |  None -> (
    try
      for i = 0 to pfile.len.(e) - 1 do
        try_change pfile e i b peer
      done;
      failwith "No flag set"
    with Ok -> ()
  )
  |  Some n -> 
    try_change pfile e n b peer



let flag ch = 
  match ch with 
  |  None -> (
    Unmatched) (*Completement arbitraire*)
  |  Some c -> c.etat


exception Kaboom

exception Casse of trileen


let casse_match reseau node peer e = 
  (*Retire peer du couplage de node
  e = true ssi on retire peer aussi si c'est un lover*)
  try
    for i = node.couplage.len.(0) -1 downto 0 do 
      let matc = node.couplage.tab.(0).(i) in 
      let f = flag matc in
      if id matc = peer.id && (e || f = Unloving) then (
        let duree = remove_nth reseau node.couplage 0 i in 
        if f = Loving then 
          reseau.tmps_vie <- duree :: reseau.tmps_vie;
        
        (match flag matc with
        |  Unloving -> (*Le couple removed n'est pas loving: il est encore dans la config normalement*)
          set_flag_in_pfile node.config 0 peer Unmatched None
        | Loving -> begin (*On a retiré un lover: il faut décrémenter num_loving et rajouter node
                    à unloving si node.num_loving était à !b*)
          if node.num_loving = !b then begin
            reseau.unloving.(reseau.len_unloving) <- Some node;
            node.ind_unloving <- reseau.len_unloving;
            reseau.len_unloving <- reseau.len_unloving +1
          end;
          node.num_loving <- node.num_loving -1
        end
        | Unmatched -> failwith "Le flag est à Unmatched dans couplage");

        raise (Casse f))
    done;
    raise Kaboom
  with Casse f -> f




let remove_from_couplages reseau node e = 
  (*Retire node de tous les couplages imparfaits
  e = true ssi on retire également node des couples loving*)
  for i = 0 to reseau.len_noeuds -1 do
    let p = nth_noeuds reseau i in
    try 
      let etat = casse_match reseau p node e in
      if not e then 
        assert(etat = Unloving)
    with Kaboom -> ()
  done

let traitement_loving node peer reseau = 
  (*Printf.printf "LOVING between %d and %d - " node.id peer.id;(*C'est une loving pair*)*)
  node.num_loving <- node.num_loving +1;
  peer.num_loving <- peer.num_loving +1;

  set_flag_in_pfile node.couplage 0 peer Loving None;
  set_flag_in_pfile peer.couplage 0 node Loving None;

  if node.num_loving = !b then begin (*Le node est complètement couplé qu'avec des paires incassables: on le retire de unloving*)
    (*Printf.printf "removing node - ";*)
    nullify_unloving reseau node; 
    remove_from_couplages reseau node false
    (*Printf.printf "Nullified \n "*)
  end;
  if peer.num_loving = !b then begin
    (*Printf.printf "removing peer - ";*)
    nullify_unloving reseau peer;
    remove_from_couplages reseau peer false
    (*Printf.printf "Nullified \n"*)
  end;
  rm_config reseau 0 node peer; (*A priori un peu bête; on devrait juste aller chercher le dernier*)
  rm_config reseau 0 peer node


let check_loving node peer reseau = (*Vérification locale*)
  if node.config.len.(0) > 0 then begin
    let best = best_config 0 node in
    if best.n.id = peer.id && (best_config 0 peer).n.id = node.id then               (*Erreur possible ici quant au best_config peer*)
      traitement_loving peer node reseau
  end




let couplement node peer reseau marque = (*On a trouvé une paire devant être couplée: 
  On ajoute la paire au couplage et 
    si la paire est loving on retire de chaque config le pair correspondant et 
    on les enlève du unloving du réseau et on réduit le unloving du reseau si ils sont complètement couplés avec des loving*)
  (*Printf.printf "Coupling: %d %d %d" node.id peer.id marque;*)

  pfile_insere reseau node.couplage 0 peer Unloving marque;
  pfile_insere reseau peer.couplage 0 node Unloving marque;
  (*print_string "Inserted";*)

  set_flag_in_pfile node.config 0 peer Unloving None;
  set_flag_in_pfile peer.config 0 node Unloving None;


  match peer.config.tab.(0).(peer.config.len.(0) -1) with 
  |  None -> (print_string "\nLe peer choisi n'a pas de config\n"; exit(1))
  |  Some c when c.n.id <> node.id -> (*print_string "Pas loving - "*) ()
  |  Some c ->
    traitement_loving node peer reseau
  
  (*;

  
  print_string "Removing two new coupled from each other's config\nNode:";
  rm_pfile node.config peer peer;
  print_string "\nPeer:";
  rm_pfile peer.config node node*)
  


let marque ch = 
  match ch with 
  |  None -> -1
  |  Some c -> c.marque


exception Found_int of int 

let marque_in_reference node peer = (*Renvoie la marque de peer dans la config.(1) de node*)
  try
    for i = 0 to node.config.len.(1) -1 do 
      match node.config.tab.(1).(i) with
      |  None -> failwith "Nannn"
      |  Some somm when somm.n.id = peer.id -> 
        raise (Found_int somm.marque)
      |  Some somm -> ()
    done;
    failwith "pas trouvé"
  with Found_int m -> m
    



let casse_match_permissif r c n = 
  (*print_string "Cassing";*)
  let traitement node peer etat = 
    if etat = Unloving then 
      set_flag_in_pfile node.config 0 peer Unmatched None
    else if etat = Loving then
      pfile_insere r node.config 0 peer Unmatched c.marque
    else 
      failwith "Unmatched dans couplage"
  in
  let etat = casse_match r c.n n true in 
  traitement c.n n etat;
  let etat = casse_match r n c.n true in
  traitement n c.n etat



let proposal node peer reseau m = (*Immonde, à nettoyer*) 
  let wno = worst node in 
  let wpo = worst peer in
  if node.couplage.len.(0) = !b && peer.couplage.len.(0) = !b then 
    match wno, wpo with 
    |  Some wn, Some wp when m >= wp.marque || m >= wn.marque ->( 
      (*On s'assure que faire un changement garantit de trouver un meilleur état*)
      (*On retire les worst mate de peer et node pour les remplacer avec un autre mate de marque str sup*)
        (*print_string "changement du couplage - ";*)


        (*Printf.printf "Removing %d's w: %d..." node.id wn.n.id;*)
        casse_match_permissif reseau wn node;
        (*Printf.printf "Removing %d's w: %d..." peer.id wp.n.id;*)
        casse_match_permissif reseau wp peer;

        couplement node peer reseau m)

    |  Some n, None | None, Some n -> failwith "Broken :/"
    | None, None -> failwith "Broken :/"
    |  _ ->    ()    (*    (*Il n'est pas intéressant de faire de changement*)
      begin
    
        Printf.printf "Pas de changement: num_loving du node: %d - num_loving du peer: %d\nCouplage_node / Couplage_peer / Config_node / Config_peer:\n" node.num_loving peer.num_loving;
        affiche_node_array node.couplage;
        affiche_node_array peer.couplage;
        affiche_node_array node.config;
        affiche_node_array peer.config
      end*)

  else if node.couplage.len.(0) = !b && marque wno <= m then
    match wno with 
    |  Some wn ->( 
      (*Printf.printf "Ajout chez peer, changement chez node - ";*)
      (*pfile_insere node.config 0 wn.n Unmatched wn.marque;*)
      assert(node.num_loving < !b);
      (*Printf.printf "Removing %d's w: %d..." node.id wn.n.id;*)
      casse_match_permissif reseau wn node;      (* On enlève le worst du couplage de node pour avoir la place pour peer (qui est mieux)*)

      couplement node peer reseau m)

    |  None -> failwith "Broken :/"

  else if peer.couplage.len.(0) = !b && marque wpo <= m then 
    match wpo with 
    |  Some wp ->( 
  
    (*Printf.printf "Ajout chez node, changement chez peer - ";*)
      (*pfile_insere node.config 0 wp.n Unmatched wp.marque;*)
      assert(peer.num_loving < !b);
      (*Printf.printf "Removing %d's w: %d..." peer.id wp.n.id;*)
      casse_match_permissif reseau wp peer;      (* On enlève le worst du couplage de node pour avoir la place pour peer (qui est mieux)*)

      couplement node peer reseau m)

    |  None -> failwith "Broken :/"

  else if peer.couplage.len.(0) <> !b && node.couplage.len.(0) <> !b then (
    (*print_string "Ajout aux deux ... ";*)
    couplement node peer reseau m
  )
  else ()
    (*Printf.printf "Pas de changement"(**)
    begin
    
      Printf.printf "Pas de changement: num_loving du node: %d - num_loving du peer: %d\nCouplage_node / Couplage_peer / Config_node / Config_peer:\n" node.num_loving peer.num_loving;
        affiche_node_array node.couplage;
        affiche_node_array peer.couplage;
        affiche_node_array node.config;
        affiche_node_array peer.config
      end*)


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
    for i = 0 to node.couplage.len.(0) -1 do 
      if peer.id = id node.couplage.tab.(0).(i) then raise Ok 
    done;
    false
  with Ok -> true

exception Ended

let initiative_best reseau = (*Réalise une initiative via stratégie best mate*)
  let node = choix_best reseau in
  assert(not (has_before node.config 0 node 0));

  if node.config.len.(0) > 0 then begin
    match node.config.tab.(0).(node.config.len.(0) -1) with
    |  None -> failwith "Erreur: le best mate du node est à None"
    |  Some best -> begin 
      if best.n.ind_unloving = -1 || best.etat = Loving then begin
        (* On retire de la config et on repioche. Le retrait d'un node cause au plus len_config repiochages -> ok complexité
          Ceci arrive quand un best couplé apparaît dans la config de node *)
        (*Printf.printf "Le peer %d a été retiré ...  " best.n.id;*)
        let _ = pfile_defile node.config 0 in 
        ()
      end
      else if best.etat = Unloving then begin
        (*Printf.printf "Le peer %d a déjà été ajouté - on check_loving ...  " best.n.id;*)
        assert(mem_couplage node best.n);
        check_loving node best.n reseau
      end
      else begin                                     (* On voit s'il est intéressant de coupler node et best -> c'est intéressant pour node, l'est-ce pour best?*)
        (*Printf.printf "Lancement proposal avec %d (%d), ... " best.n.id best.marque;*)
        proposal node best.n reseau best.marque
      end
    end
  end

  else begin 
    (*Printf.printf "Absurdité: node %d a une config de taille %d\n" node.id node.config.len.(0);*)
    (*affiche_reseau reseau;*)
    if !b >= reseau.len_unloving then 
      raise Ended
    else 
      failwith "Absurdité"
  end
  (*begin 
    Printf.printf "Ajout de nodes dans la config du node prcq il y en a plus ...  ";
  let c = connections_init reseau in 
  (* Copier les nouveaux éléments *)
  for i = 0 to c.len - 1 do 
    match c.tab.(i) with
    | Some n, false when n.id != node.id -> 
        pfile_insere node.config n false;
        pfile_insere n.config node false;
    | _ -> ()
  done;
  Printf.printf "New config has length %d ... " node.config.len;
  reseau.len_unloving <- 0;
end*)




let protocol reseau = 
  (*print_string "\n\nDébut protocol\n";*)
  try
    while reseau.len_unloving > 0 do 
      (*Printf.printf "Lancement à len_unloving = %d ... " reseau.len_unloving; *)
      initiative_best reseau;
      (*print_string "Initiative finie\n\n";*)
      flush_all ();
    done
  with Ended -> ()(*(print_string "Ended premature...\n"; flush_all ());*)
  (*print_string "Convergence réussie"*)
  

let protocol_vtest reseau = 
  print_string "Début protocol\n";
  for i = 0 to 100 do 

    Printf.printf "Lancement à len_unloving = %d ..." reseau.len_unloving; 
    initiative_best reseau;
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


let marque node = 
  node.marque


let noeud c = 
  c.n


let get_b () = 
  !b

let set_b new_b = 
  b := new_b


let id_n node = 
  node.id


let tmps_vie r = 
  r.tmps_vie


let affiche_unloving reseau = 
  print_string "Unloving: ";
  for i = 0 to reseau.len_unloving -1 do 
    match reseau.unloving.(i) with 
    |  None -> ()
    |  Some n -> Printf.printf "Some %d, " n.id
  done



exception Found_node of node
exception Found of int



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


let rank_in pfile e id = (*Renvoie le rang de id +1 dans pfile si id y est, 0 sinon*)
  try
    for i = 0 to pfile.len.(e) -1 do 
      match pfile.tab.(e).(i) with 
      |  Some node when node.n.id = id -> raise (Found (pfile.len.(e) -i)) 
      |  _ -> ()
    done;
    0
  with Found i -> i


let f = float_of_int


let satisfaction r = (*Quantifie une mesure s de la qualité du couplage
                      obtenu dans r comparé aux config de r0
                      0 <= s <= 1
              Cette version ne fonctionne pas si on retire des noeuds au cours du calcul
              du couplage*)
  let sat = ref 0. in 
  for i = 0 to r.len_noeuds -1 do 
    match r.noeuds.(i) with 
    | None -> failwith "Pas possible"
    | Some node -> 
        let static = (f node.couplage.len.(0)) /. (f !b) in 
        sat := !sat +. static;
        for i = 0 to node.couplage.len.(0) -1 do
          match node.couplage.tab.(0).(i) with 
          |  None -> ()
          |  Some coupled -> 
            let esp = f ((rank_in node.config 1 coupled.n.id) - (node.couplage.len.(0) -i)) in 
            let added = esp /. ((f node.config.len.(1)) *. (f !b)) in 
            assert(added <= 1.);
            sat := !sat -. added
        done
  done; 
  let s = !sat /. (f r.len_noeuds) in 
  s



(*      Quelques fonctions pour copier un réseau      *)

let pfile_init () = 
  {tab = [|Array.make max_config None; Array.make max_config None|];
  len = [|0; 0|];
  }


let node_init_without_pfile id ind_n ind_u n_lov = 
  {id = id;
  ind_noeuds = ind_n;
  ind_unloving = ind_u;
  config = pfile_init ();
  couplage = pfile_init ();
  num_loving = n_lov;}



let copy_tab r pf1 pf2 e r1 = 
  let l1 = pf1.len.(e) in 
  let l2 = pf2.len.(e) in
  for i = 0 to l1 -1 do 
    pf1.tab.(e).(i) <- None
  done;
  pf1.len.(e) <- 0;
  assert(pf1.tab.(e).(l1) = None);
  for i = 0 to l2 -1 do 
      match pf2.tab.(e).(i) with 
      |  None -> 
        ()
      |  Some ch -> 
        let n = nth_noeuds r1 ch.n.ind_noeuds in
        pfile_insere r pf1 e n ch.etat ch.marque
  done



let copy_pfiles r pf1 pf2 r1 = (*copie pf2 dans pf1. pf1 est issu de r1*)
  for e = 0 to 1 do
    copy_tab r pf1 pf2 e r1
  done
  

let reseau_copy r0 = 
  let r = reseau_init () in 
  r.len_unloving <- r0.len_unloving;
  r.len_noeuds <- r0.len_noeuds;
  r.num <- r0.num;
  r.tmps_vie <- r0.tmps_vie;
  r.step <- r0.step; 

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
      copy_pfiles r n1.couplage n2.couplage r;
      copy_pfiles r n1.config n2.config r
  done;

  r



(*    ----- Code propre à Couplage_adaptatif -----  *)




let marque_in_config e node peer = (*Renvoie la marque de peer dans la config de node
  si peer y est, et lève Ok sinon*)
  try
    for i = 0 to node.config.len.(e) -1 do 
      match node.config.tab.(e).(i) with 
      |  None -> failwith "BLABLABLA"
      |  Some n when n.n.id = peer.id -> 
          raise (Found n.marque)
      |  Some n -> ()
    done;
    raise Ok
  with Found m -> m



let modif_lovers r node = (*S'assure que toutes les paires formées
  ont une marque inférieure à celle proposée par un couplage avec node.
  On casse celles pour lesquelles ce n'est pas le cas.
  On renvoie le nombre de modifications.*)
  let c = ref 0 in
  for i = 0 to r.len_noeuds -1 do 
    match r.noeuds.(i) with 
    |  None -> failwith "NONONO"
    |  Some peer -> 
      try
        let m = marque_in_config 1 peer node in (*normalement ça y est*)
        for j = peer.couplage.len.(0) -1 downto 0 do 
          match peer.couplage.tab.(0).(j) with 
          |  None -> failwith "BOUBOUBOU"
          |  Some ch -> 
            if ch.marque < m then (
              let _ = casse_match r peer ch.n true in
              let _ = casse_match r ch.n peer true in
              incr c
            )
            
        done
      with Ok -> assert(node.id = peer.id) 
  done;

  (*Puis il faut rajouter des noeuds dans les configs des gens dans 
  unloving. Potentiellement, à l'issue du dernier calcul de couplage, 
  leur config.(0) s'est faite vidée. Concrètement on copie depuis config.(1)*)

  (*Et enfin réactualiser les flags car le tableau global n'est pas 
  à jour en flags*)

  let copy_tab_only_unloving pf e1 e2 r = 
    for i = 0 to pf.len.(e1) -1 do 
      pf.tab.(e1).(i) <- None
    done;
    pf.len.(e1) <- 0;
    for i = 0 to pf.len.(e2) -1 do 
      match pf.tab.(e2).(i) with 
      |  None -> ()
      |  Some ch -> 
        if ch.n.ind_unloving <> -1 then 
          pfile_insere r pf e1 ch.n Unmatched ch.marque;
    done;
  in

  for i = 0 to r.len_unloving -1 do 
    match r.unloving.(i) with 
    |  None -> failwith ""
    |  Some node -> (
      let prev = Array.copy node.couplage.tab.(0) in 
      let len = node.couplage.len.(0) in 
      copy_tab_only_unloving node.config 0 1 r;
      (*Réactualisation des flags*)
      for i = 0 to len -1 do 
        match prev.(i) with 
        |  None -> failwith "zoubizou"
        |  Some peer -> 
          try 
            set_flag_in_pfile node.config 0 peer.n peer.etat None
            (*Printf.printf " chez %d à %s\n" node.id (tril_to_string peer.etat)*)
          with Failure(s) -> ()
            (*print_string "...not in...\n"*)
      done;
      (*Réactualisation des num_loving: a priori pas nécessaire mais 
      ça semble foirer*)
      let new_num = ref 0 in 
      for i = 0 to node.couplage.len.(0) -1 do 
        match node.couplage.tab.(0).(i) with 
        |  None -> failwith "Kadaboom"
        |  Some ch -> 
          if ch.etat = Loving then 
            incr new_num
      done;
      (*Printf.printf "New_num for %d: %d\n" node.id !new_num;*)
      node.num_loving <- !new_num
      
          
      )
  done;
  !c



let node_add_rand p r = (*Ajoute un noeud aléatoire
  On renvoie le nombre de couples modifiés lors de modif_lovers
  *)
  let n = node_init p r in
  let c = modif_lovers r n in
  r.step <- r.step +1;
  c



let retrieve_tmps r = (*Récupère les temps de vie de toutes les arềtes présentes dans les couplages*)
  for i = 0 to r.len_noeuds -1 do 
    match r.noeuds.(i) with 
    |  None -> failwith ""
    |  Some n -> 
      for j = 0 to n.couplage.len.(0) -1 do 
        match n.couplage.tab.(0).(j) with 
        |  None -> failwith ""
        |  Some p -> 
          r.tmps_vie <- (r.step - p.tmps_debut) :: r.tmps_vie 
      done;
  done


let node_del reseau node = 
  (*Retrait de node de reseau et renvoie le nombre de couples brisés par ce retrait
  On retire des versions de travail ET globale*)
  let c = ref (node.couplage.len.(0)) in
  let arr = Array.copy node.couplage.tab.(0) in
  remove_from_couplages reseau node true;
  for i = 0 to !c-1 do 
    match arr.(i) with 
    |  None -> failwith ""
    |  Some peer -> 
      c := !c + modif_lovers reseau node 
  done;
  for i = 0 to reseau.len_noeuds -1 do
    match reseau.noeuds.(i) with 
    |  None -> failwith "Nan"
    |  Some n when n.id <> node.id -> 
      rm_config reseau 1 n node;
      (try 
        rm_config reseau 0 n node
      with Failure(s) -> ());
    |  _ -> ()
  done;
  nullify_noeuds reseau node;
  if node.ind_unloving <> -1 then 
    nullify_unloving reseau node;
  reseau.step <- 1+reseau.step;
  !c

