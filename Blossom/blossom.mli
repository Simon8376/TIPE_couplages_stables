type graph = int list array
type matching = int option array 

exception Fin of bool
exception Found of matching

val empty_matching : graph -> matching 
val mate : matching -> int -> int -> unit 
val unmate : matching -> int -> unit 
val free_vertices : matching -> int list 
val symdiff : matching -> matching -> unit 
val composante : graph -> int array 

val augmenting_path : graph -> matching -> matching -> bool 
val best_matching : graph -> matching 

val bfs : graph -> matching -> int array 
val augmenting_path_bis : graph -> matching -> matching -> bool

val afficher_tab : matching -> unit 
val afficher_tab_int : int array -> unit
val afficher_liste_int : int list -> unit 
val afficher_tab_bool : bool array -> unit 

