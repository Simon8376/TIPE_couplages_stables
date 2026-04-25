val max_noeuds : int
val max_marque : int
val max_config : int
val b : int ref
type num = { mutable num : int; }
val num : num
type trileen = Unmatched | Unloving | Loving
type node = {
  id : int;
  mutable ind_noeuds : int;
  mutable ind_unloving : int;
  config : pfile;
  couplage : pfile;
  mutable num_loving : int;
}
and pfile = { mutable tab : champ option array; mutable len : int; }
and champ = { n : node; e : trileen; marque : int; }
type reseau = {
  mutable noeuds : node option array;
  mutable len_noeuds : int;
  mutable len_unloving : int;
  mutable unloving : node option array;
}
exception Ok
val id : champ option -> int
val tril_to_string : trileen -> string
val affiche_node_array : pfile -> unit
val agrandit_tab : 'a option array -> int -> 'a option array
val switch : pfile -> int -> int -> unit
val has_before : pfile -> node -> int -> bool
val pfile_insere : pfile -> node -> trileen -> int -> unit
val pfile_defile : pfile -> champ option
val est_trie : pfile -> bool
val worst : node -> champ option
val empty_pfile : unit -> pfile
val affiche_liste_int : int list -> unit
val randint : int -> int
val knuth_fischer_yates : 'a array -> unit
val nth_unloving : reseau -> int -> node
val connections_init : reseau -> pfile
val connections_init_complet : reseau -> pfile
val node_init : reseau -> node
val node_init_noedge : reseau -> node
val reseau_init_noedge : int -> reseau
val reseau_init_cycles : int -> reseau
val nth_noeuds : reseau -> int -> node
val remove_nth : pfile -> int -> unit
val rm_config : node -> node -> unit
val best_config : node -> champ
val nullify_noeuds : reseau -> node -> unit
val affiche_reseau : reseau -> unit
val affiche_tab : node option array -> unit
val nullify_unloving : reseau -> node -> unit
val node_del : reseau -> node -> unit
val reseau_init : unit -> reseau
val mem_lover : node -> node -> bool
val try_change : pfile -> int -> trileen -> node -> unit
val set_flag_in_pfile : pfile -> node -> trileen -> int option -> unit
val flag : champ option -> trileen
val remove_from_couplages : reseau -> node -> unit
val traitement_loving : node -> node -> reseau -> unit
val check_loving : node -> node -> reseau -> (int, int) Hashtbl.t -> unit
val couplement :
  node -> node -> reseau -> int -> int -> (int, int) Hashtbl.t -> unit
val casse_match : node -> node -> unit
val marque : champ option -> int
val proposal :
  node -> node -> reseau -> int -> int -> (int, int) Hashtbl.t -> unit
val launch_initiative :
  reseau -> node -> (int, int) Hashtbl.t -> unit
val choix_best : reseau -> node
val marque : champ option -> int
val mem_couplage : node -> node -> bool
val initiative_best : reseau -> (int, int) Hashtbl.t -> unit
val protocol : reseau -> unit
val protocol_vtest : reseau -> unit
val len_reseau : reseau -> int
val config : node -> pfile
val couplage : node -> pfile
val lr : node -> int
val marque : champ -> int
val noeud : champ -> node
val get_b : unit -> int
val set_b : int -> unit
