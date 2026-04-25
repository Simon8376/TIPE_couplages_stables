module T = Test_blossom

let () = 
   T.test_mate_unmate ();
   T.test_symdiff ();
   T.test_augmenting_path ();
   T.test_augmenting_path_bis ();
   T.test_bfs ();
   T.test_best_matching ()
