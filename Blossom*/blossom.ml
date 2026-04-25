type graph = int list array 

type matching = int option array 

let empty_matching g = 
  Array.make (Array.length g) None


exception Fin of bool
exception Found of matching




let mate matching v1 v2 = 
  matching.(v1) <- Some v2; 
  matching.(v2) <- Some v1



let unmate matching v1 =
  match matching.(v1) with 
  | Some v2 -> matching.(v2) <- None; matching.(v1) <- None 
  | None -> ()



let free_vertices matching = 
  let l = ref [] in 
  for i = 0 to Array.length matching -1 do
    if None = matching.(i) then l := i :: !l 
  done;
  !l



let symdiff matching path = 
  let t = Array.length path in 
  for i = 0 to t-1 do 
    if path.(i) <> None then 
      matching.(i) <- path.(i) 
  done



let augmenting_path g matching path = 
  let t = Array.length g in 
  let l = free_vertices matching in 
  let vus = Array.make t false in
  let rec traitement v = 
    vus.(v) <- true;
      List.iter (fun j -> if not vus.(j) then begin
                            mate path v j;
                            dfs j v;
                            unmate path v;
                          end) g.(v);
    vus.(v) <- false;
  and dfs i last = 
    match matching.(i) with 
    |  None when last <> -1 -> raise (Found path) 
    |  Some v -> 
      vus.(v) <- true;
      traitement v;
      vus.(v) <- true
    |  None ->
      traitement i
  in 
  try 
    List.iter (fun i -> dfs i (-1)) l;
    false
  with Found path -> true



let afficher_tab tab = 
  for i = 0 to Array.length tab -1 do 
    match tab.(i) with 
    |  None -> print_string "None, "
    |  Some v -> Printf.printf "Some %d, " v 
  done;
  print_newline()




let best_matching g = 
  let path = empty_matching g in 
  let matching = empty_matching g in
  while augmenting_path g matching path do 
    symdiff matching path;
    for i = 0 to Array.length g -1 do 
      path.(i) <- None 
    done 
  done;
  matching






let composante g = 
  let compo = Array.make (Array.length g) (-1) in 
  let vus = Array.make (Array.length g) false in
  let rec dfs i col = 
    if not vus.(i) then begin
      vus.(i) <- true;
      compo.(i) <- col;
      List.iter (fun j -> dfs j (1-col)) g.(i) 
    end
  in
  for i = 0 to (Array.length g -1) do 
    dfs i 0
  done;
  compo



let afficher_tab_int tab = 
  for i = 0 to Array.length tab -1 do 
    Printf.printf "%d, " tab.(i)
  done  



let rec afficher_liste_int l = 
  match l with 
  |  [] -> ()
  |  h :: t -> Printf.printf "%d, " h; afficher_liste_int t



let bfs g matching = (*On compte sur le fait que lorsque la distance commence à
                      être trop grande, elle le sera pour tous les sommets suivants
                      C'est l'idée du bfs*)
  let col = composante g in 
  let t = Array.length g in
  let l = free_vertices matching in
  let distance = Array.make t (int_of_float infinity) in 
  let q = Queue.create () in
  List.iter (fun i -> if col.(i) = 0 then distance.(i) <- 0) l;

  for i = 0 to t-1 do 
    if col.(i) = 0 && List.mem i l then begin 
      let vus = Array.make t false in 
      Queue.push (i, 0) q;
      while not (Queue.is_empty q) do 
        let x, d = Queue.take q in 
        if d < distance.(x) then
          distance.(x) <- d;
          List.iter (fun y -> 
            if not vus.(y) then begin 
              vus.(y) <- true;
              distance.(y) <- min distance.(y) (distance.(x) +1);
              match matching.(y) with
                |  None -> ()
                |  Some z -> if not vus.(z) then (
                                vus.(z) <- true;
                                Queue.push (z, distance.(y) +1) q)
          end) g.(x)
      done
    end
  done;
  distance




let afficher_tab_bool tab = 
  for i = 0 to Array.length tab -1 do 
    Printf.printf "%s, " (if tab.(i) then "true" else "false")
  done;
  print_newline ()



let rec augmenting_path_bis g matching path = 
  let t = Array.length g in 
  let l = free_vertices matching in 
  let vus = Array.make t false in
  let distance = bfs g matching in
  let compo = composante g in

  let rec traitement v = 
      List.iter (fun j -> if not vus.(j) && distance.(j) = distance.(v) +1 then begin
                            mate path v j;
                            dfs j v;
                            unmate path v;
                          end) g.(v);

  and dfs i last = 
    vus.(i) <- true;
    match matching.(i) with 
    |  None when last <> -1 -> 
      raise (Found path) 
    |  Some v  when distance.(v) = distance.(i) +1 -> 
      vus.(v) <- true;
      traitement v;
      vus.(v) <- false;
    |  Some v -> ()
    |  None ->
      traitement i;
    vus.(i) <- false
  in 

  let b = ref true in
  while !b do
    try 
      List.iter (fun i -> if compo.(i) = 0 && not vus.(i) then (dfs i (-1); vus.(i) <- true)) l;
      b := false
    with Found path -> 
      b := false;
      for i = 0 to t-1 do 
        if compo.(i) = 0 && not vus.(i) then b := true 
      done
  done;
  List.exists (fun n -> path.(n) <> None) (List.init t (fun i -> i))





(*Question 10:
Complexité du parcours en largeur: 
 - composante et free_vertices: O(n) + O(n)
 - Création des structures: O(n)
 - O(n) vu que le graphe est biparti donc chaque sommet rencontré est croisé pas beaucoup
 de fois (tkt)
Complexité du parcours en profondeur:
 - O(n + m) (surement pour des raisons obscures)
Complexité de l'amélioration: 
 - O(n)
*)

(*Question 11:
NON vu que le graphe est parcourut dans son entièreté à chaque étape
et qu'il valide un chemin dès qu'il en trouve un -> il aurait donc déjà 
détecté le chemin avant*)

(*Question 12:
Chaque étape détecte des chemins améliorants de taille croissante
Alors à l'étape |V|^1/2 les chemins sont de taille au moins
|V|^1/2. On a au plus une arête de couplage par 2 sommet 
donc au plus (V/2) / (|V|)^1/2 <= |V|^1/2 différents couplages*)

(*Question 13
L'étape 1 trouve au moins 1 chemin de taille 1
L'étape 2 trouve au moins 1 chemin de taille 2. On a alors un couplage
de taille 1+2
Plus généralement, au bout de n étapes, on a un couplage de taille
SUM(k), 1 <= k <= n, c'est-à-dire un couplage de taille n(n-1)/2
La taille du couplage étant majorée par n, on fait au plus SQRT(n) étapes

La complexité est alors de O((n+m)n^1/2) ou qqch comme ca*) 





(*Couplage parfait dans graphes bipartis pondérés complets équilibrés*)

(*Question 14: à chaque sommet on attribue SUM(poids de chaqune de ses arêtes)
Alors l(u) + l(v) >= 2*w(u, v)*)

(*Soit un chemin alternant dans l'arbre alternant dans le graphe initial*)