type node
type pfile
type reseau
type champ
type trileen
exception Ok

val agrandit_tab : 'a option array -> int -> 'a option array 

val switch: pfile -> int -> int -> unit 
val pfile_insere: pfile -> node  -> trileen -> int -> unit 
val pfile_defile: pfile -> champ option

val empty_pfile: unit -> pfile
val affiche_liste_int: int list -> unit
val worst: node -> champ option
val est_trie: pfile -> bool

val connections_init: reseau -> pfile
val node_init: reseau -> node 
val nullify_noeuds: reseau -> node -> unit
val nullify_unloving: reseau -> node -> unit
val node_del: reseau -> node -> unit
val reseau_init: unit -> reseau

val couplement: node -> node -> reseau -> int -> unit
val proposal: node -> node -> reseau -> int -> unit 
val choix_best: reseau -> node
val initiative_best: reseau -> unit 
val protocol: reseau -> unit 
val protocol_vtest: reseau -> unit

val has_before: pfile -> node -> int -> bool
val affiche_reseau: reseau -> unit
val len_reseau: reseau -> int
val nth_noeuds: reseau -> int -> node
val nth_unloving: reseau -> int -> node
val config: node -> pfile
val best_config: node -> champ
val affiche_node_array: pfile -> unit
val couplage: node -> pfile
val lr: node -> int
val marque: champ -> int
val id: champ option -> int
val noeud: champ -> node
val get_b: unit -> int
val set_b: int -> unit
