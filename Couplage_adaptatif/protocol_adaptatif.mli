type node
type pfile
type reseau
type champ
type trileen
exception Ok

val affiche_liste_int: int list -> unit

val node_init: float -> reseau -> node 
val node_del: reseau -> node -> int
val reseau_init: unit -> reseau

val protocol: reseau -> unit 
val protocol_vtest: reseau -> unit

val affiche_reseau: reseau -> unit
val len_reseau: reseau -> int
val nth_noeuds: reseau -> int -> node
val nth_unloving: reseau -> int -> node
val config: node -> pfile
val best_config: int -> node -> champ
val affiche_node_array: pfile -> int -> unit
val couplage: node -> pfile
val lr: node -> int
val marque: champ -> int
val id: champ option -> int
val id_n: node -> int
val noeud: champ -> node
val get_b: unit -> int
val set_b: int -> unit
val reseau_copy: reseau -> reseau 
val satisfaction: reseau -> float
val modif_lovers: reseau -> node -> int
val node_add_rand: float -> reseau -> int
