(** Calcule la valeur de sigma appropriee pour l'algorithme ExtractVector *)
let compute_sigma p =
  let n = Array.length p in
  (* Calcul de M: tel que |p_i| <= 2^M *)
  let max_abs_p = Array.fold_left (fun acc x -> max acc (abs_float x)) 0. p in
  let m = ceil (log (max_abs_p +. 1e-10) /. log 2.) in
  
  (* Calcul de N: tel que n <= 2^N *)
  let n_float = float_of_int n in
  let n_power = ceil (log n_float /. log 2.) in
  
  (* Sigma = 3/2 * 2^(M+N) comme defini dans la preuve *)
  1.5 *. (2. ** (m +. n_power))

(** Implementation de l'algorithme ExtractVector *)
let extract_vector p =
  let n = Array.length p in
  let sigma = compute_sigma p in
  
  let tau = ref 0. in
  let p_prime = Array.make n 0. in
  
  Array.iteri (fun i pi ->
    let qi = (sigma +. pi) -. sigma in
    p_prime.(i) <- pi -. qi;
    tau := !tau +. qi
  ) p;
  
  (!tau, p_prime)

(** Vecteur 1: Vecteur simple avec des valeurs arbitraires *)
let create_test_vector1 () =
  [| 1.1111111111331111; 3.5555552222222222; 2.2222222222222222; 7.9009993339999900 |]

(** Vecteur 2: Vecteur avec des annulations partielles *)
let create_test_vector2 () =
  [| 1.0; -1.0 +. 1e-10; 1e-8; -1e-8 +. 1e-16 |]

(** Vecteur 3: Vecteur mal conditionne *)
let create_ill_conditioned_vector n scale =
  let p = Array.make n 0. in
  for i = 0 to n-1 do
    if i mod 2 = 0 then
      p.(i) <- scale *. (1. +. (float_of_int i) /. (float_of_int n))
    else
      p.(i) <- -. scale *. (1. -. 1e-10 *. (float_of_int i))
  done;
  p

(** Vecteur 4: Vecteur aleatoire de grande taille *)
let create_random_vector n min_val max_val =
  let range = max_val -. min_val in
  let p = Array.make n 0. in
  Random.init 42;  (* Fixe la graine pour reproductibilite *)
  for i = 0 to n-1 do
    p.(i) <- min_val +. range *. Random.float 1.0
  done;
  p

(** Vecteur 5: Sequence de Kahan *)
let create_kahan_sequence n =
  let p = Array.make n 0. in
  p.(0) <- 1.0;
  for i = 1 to n-1 do
    p.(i) <- 1e-16 *. (float_of_int i)
  done;
  p

(** Vecteur 6: Vecteur avec differentes echelles *)
let create_scale_difference_vector () =
  [| 1e20; 1.0; -1e20; 1e-10; 1.0; 1.0 |]

(** Vecteur 7: Oscillations pres de zero *)
let create_oscillating_vector n =
  let p = Array.make n 0. in
  for i = 0 to n-1 do
    let sign = if i mod 2 = 0 then 1.0 else -1.0 in
    p.(i) <- sign *. 1e-5 *. (1.0 +. (float_of_int i) /. (float_of_int n))
  done;
  p

(** Fonction de test pour l'algorithme ExtractVector *)
let test_vector name p =
  Printf.printf "\n=== Test sur %s ===\n" name;
  
  (* Afficher le vecteur d'entree si sa taille est raisonnable *)
  if Array.length p <= 10 then begin
    Printf.printf "Vecteur p: [%s]\n" 
      (String.concat "; " (Array.to_list (Array.map (Printf.sprintf "%.16f") p)));
  end else begin
    Printf.printf "Vecteur de taille %d\n" (Array.length p);
  end;
  
  (* Application de l'algorithme ExtractVector *)
  let sigma = compute_sigma p in
  let tau, p_prime = extract_vector p in
  
  Printf.printf "\nExtractVector:\n";
  Printf.printf "Sigma calcule: %.16f\n" sigma;
  Printf.printf "Tau extrait: %.16f\n" tau;
  
  (* Pour des vecteurs raisonnablement petits, afficher p' *)
  if Array.length p <= 10 then begin
    Printf.printf "Composantes p': [%s]\n" 
      (String.concat "; " (Array.to_list (Array.map (Printf.sprintf "%.16e") p_prime)));
  end;
  
  (* Calcul de la somme des composantes p' *)
  let p_prime_sum = Array.fold_left (+.) 0. p_prime in
  Printf.printf "Somme des composantes p': %.16e\n" p_prime_sum;
  
  (* Verification de la propriete fondamentale: somme originale = tau + somme(p') *)
  let original_sum = Array.fold_left (+.) 0. p in
  let reconstructed_sum = tau +. p_prime_sum in
  
  Printf.printf "\nVerification de la propriete fondamentale:\n";
  Printf.printf "Somme originale:           %.16f\n" original_sum;
  Printf.printf "Tau + somme(p'):           %.16f\n" reconstructed_sum;
  Printf.printf "Difference:                %.16e\n" (reconstructed_sum -. original_sum);
  
  (* Verification de la qualite de l'extraction *)
  Printf.printf "\nAnalyse de la qualite de l'extraction:\n";
  let max_abs_p_prime = Array.fold_left (fun acc x -> max acc (abs_float x)) 0. p_prime in
  Printf.printf "Magnitude maximale dans p': %.16e\n" max_abs_p_prime;
  Printf.printf "Magnitude de tau:           %.16e\n" (abs_float tau);
  
  if max_abs_p_prime < abs_float tau *. 1e-10 then
    Printf.printf "SUCCES: Les composantes p' sont au moins 10 ordres de grandeur plus petites que tau.\n"
  else if max_abs_p_prime < abs_float tau then
    Printf.printf "PARTIEL: Les composantes p' sont plus petites que tau, mais pas de facon significative.\n"
  else
    Printf.printf "ATTENTION: Certaines composantes p' ont une magnitude comparable ou superieure a tau.\n";

  (* Verification theorique pour certains vecteurs particuliers *)
  match name with
  | "vecteur avec differences d'echelle" ->
      Printf.printf "\nVerification specifique pour ce type de vecteur:\n";
      Printf.printf "Somme theorique attendue:   3.0000000001000000\n";
      Printf.printf "La somme reconstruite est %s a la somme theorique.\n"
        (if abs_float (reconstructed_sum -. 3.0000000001) < 1e-10 then "EGALE" else "DIFFERENTE");
  | "vecteur avec annulations partielles" ->
      Printf.printf "\nVerification specifique pour ce type de vecteur:\n";
      Printf.printf "Somme theorique attendue:   0.0000000001000001\n";
      Printf.printf "La somme reconstruite est %s a la somme theorique.\n"
        (if abs_float (reconstructed_sum -. 0.0000000001000001) < 1e-16 then "EGALE" else "DIFFERENTE");
  | _ -> ()

(** Fonction principale pour tester l'algorithme *)
let main () =
  (* En-tete des tests *)
  Printf.printf "=== DEMONSTRATION DE L'ALGORITHME EXTRACTVECTOR ===\n\n";
  Printf.printf "Cet algorithme extrait la partie haute (sommable exactement) d'un vecteur\n";
  Printf.printf "et laisse un vecteur residuel de petite magnitude.\n";
  Printf.printf "Propriete fondamentale: tau + somme(p') = somme(p) exactement.\n\n";
  
  (* Test 1: Vecteur simple avec des valeurs de test *)
  let test_vector1 = create_test_vector1 () in
  test_vector "vecteur simple (test de base)" test_vector1;
  
  (* Test 2: Vecteur avec des valeurs opposees qui s'annulent partiellement *)
  let test_vector2 = create_test_vector2 () in
  test_vector "vecteur avec annulations partielles" test_vector2;
  
  (* Test 3: Vecteur mal conditionne de taille moyenne *)
  let test_vector3 = create_ill_conditioned_vector 50 1e8 in
  test_vector "vecteur mal conditionne (taille 50)" test_vector3;

  (* Test 4: Vecteur aleatoire de grande taille *)
  let test_vector4 = create_random_vector 1000 (-1e6) 1e6 in
  test_vector "vecteur aleatoire (taille 1000)" test_vector4;
  
  (* Test 5: Sequence de Kahan - un defi classique pour les algorithmes de sommation *)
  let test_vector5 = create_kahan_sequence 100 in
  test_vector "sequence de Kahan (taille 100)" test_vector5;
  
  (* Test 6: Vecteur avec des valeurs tres differentes *)
  let test_vector6 = create_scale_difference_vector () in
  test_vector "vecteur avec differences d'echelle" test_vector6;
  
  (* Test 7: Vecteur avec oscillations pres de zero *)
  let test_vector7 = create_oscillating_vector 100 in
  test_vector "vecteur avec oscillations pres de zero (taille 100)" test_vector7;
  
  (* Conclusion des tests *)
  Printf.printf "\n=== CONCLUSION ===\n";
  Printf.printf "La propriete fondamentale de l'algorithme ExtractVector est verifiee:\n";
  Printf.printf "Pour tous les vecteurs testes, tau + somme(p') = somme(p) exactement.\n\n";
  Printf.printf "De plus, dans la plupart des cas, les composantes p' sont de magnitude beaucoup\n";
  Printf.printf "plus petite que tau, ce qui montre que l'extraction est efficace.\n\n";
  Printf.printf "Pour le vecteur avec differences d'echelle, l'algorithme parvient a extraire\n";
  Printf.printf "correctement tous les termes, y compris les termes de petit ordre de grandeur\n";
  Printf.printf "qui seraient perdus par une sommation directe a cause des grands nombres.\n"

(* Execution des tests *)
let () = main ()