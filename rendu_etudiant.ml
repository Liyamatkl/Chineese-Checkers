(*CHAN Michel &&
AIT OUAKLI Liyam*)

let dim = 3 ;;

type case = int * int * int ;;

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
   (* Case libre du plateau *)
  | Libre
  (* Case en dehors du plateau, utile pour l'affichage *)
  | Dehors
  | Nombre of int
  (* Pour mettre des noms *)
  | Nom of string
;;

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (* Pour mettre des noms *)
  | Nom s -> s
;;

type case_coloree = case * couleur ;;

type configuration = case_coloree list * couleur list ;;

type coup = Du of case * case | Sm of case list ;;

let configuration_initial = ([], [Vert; Jaune; Rouge; Noir; Bleu; Marron]) ;;


let liste_joueurs (_, l) = l ;;


let quelle_couleur _ _ = Libre ;;

let mis_a_jour_configuration _ _ = Error "To do" ;;

let gagnant _ = Libre ;;

(* Question n°1
1. i < -dim : l'ensemble des cases dans le camp joueur situé au Sud
2. i > dim : l'ensemble des cases dans le camp joueur situé au Nord
3. j < −dim : l'ensemble des cases dans le camp du joueur situé au Nord-Ouest
4. (i, j, k) = (2dim, −dim, −dim) : la case se trouvant au sommet du jeu au Nord
5. i ≥ −dim ∧ j ≥ −dim ∧ k ≥ −dim : Les cases ne se trouvant pas dans le camp des joueurs au Nord-Ouest et au Sud et Nord-Est)*

let est_dans_losange c = let (i,j,k) = c in if i>= -(2*dim) && i<= (2*dim && j >= -dim && j <= dim && k >= -dim && k <= dim then true else false ;;

(Question n°3
Dans la fonction ci-dessous nous avons choisi d'utiliser l'union de deux grands triangles, 
le sommet principal du premier tend vers le nord tandis que l'autre sommet principal du deuxième triangle tend vers le sud *)

let est_dans_losange c = let (i,j,k) = c in if i>= -(2*dim) && i<= (2*dim) && j >= -dim && j <= dim && k >= -dim && k <= dim then true else false ;;

let est_dans_etoile c =
  let (i, j, k) = c in
    if (i >= (-dim) && j >= (-dim) && k >= (-dim) || i <= dim && j <= dim && k <= dim) && (i + j + k = 0) then
      true
    else
      false
;;

let rec couleur_case case configuration =
  let (case_colorees, couleurs) = configuration in
    match case_colorees with
      | [] -> Libre 
      | head::tail -> let ((i, j, k), couleur), (case_i, case_j, case_k) = (head, case) in
          if i = case_i && j = case_j && k = case_k then couleur
          else couleur_case case (tail, couleurs)
;;

let quelle_couleur case configuration =
  if (est_dans_etoile case) then
    couleur_case case configuration
  else
    Dehors
;;

let tourne_case_aux c = 
  let (i, j, k) = c in
    (-k, -i, -j)
;;

let rec tourne_case m c = 
  if m <> 0 then
    tourne_case (m-1) (tourne_case_aux c)
  else
    c
;;

let rec tourne_config conf =
  let (case_colorees, couleurs) = conf in
    match case_colorees with
      | [] -> if couleurs = [] then
                ([], [])
              else ([], (List.tl couleurs)@[(List.hd couleurs)])
      | head::tail -> let (tmp1, tmp2),(tmp3, tmp4) = tourne_config (tail, couleurs), head in 
        (((tourne_case 1 tmp3), tmp4)::tmp1, tmp2)
;;

let sont_cases_voisines case1 case2 =
  let ((case1_i, case1_j, case1_k),(case2_i, case2_j, case2_k)) = (case1, case2) in
    let (case3_i, case3_j, case3_k) = (case1_i - case2_i, case1_j - case2_j, case1_k - case2_k) in
      (abs case3_i) + (abs case3_j) + (abs case3_k) == 2 && (case3_i + case3_j + case3_k = 0)
;;

let rec case_dans_config case configuration = 
  let (case_colorees, couleurs ) = configuration in
    match case_colorees with 
      []-> false
      |head::tail -> let (((case1_i,case1_j,case1_k),couleurs ),(case2_i,case2_j,case2_k)) = (head,case) in 
        if case1_i<>case2_i || case1_j <> case2_j || case1_k<> case2_k then case_dans_config case (tail,couleurs)
        else true;;

let rec quelle_couleur case configuration= 
  let (case_colorees, couleurs ) = configuration in
    if est_dans_etoile case then
      match case_colorees with
      []-> Libre
      |head::tail -> let (((case1_i,case1_j,case1_k),couleur ),(case2_i,case2_j,case2_k)) = (head,case) in
        if case1_i<>case2_i || case1_j<>case2_j || case1_k<>case2_k then quelle_couleur case (tail,couleurs)
        else couleur
  else
    Dehors ;;


let remplir_triangle configuration couleur case = 
let rec remplir_triangle_1 configuration couleur case= 
let remplir_triangle_aux configuration couleur case= 
let rec remplir_triangle_aux_1 configuration couleur case n= 
  let (i,j,k) = case in 
    if (i+n) < (-dim) then remplir_triangle_aux_1 ([((i+n,j-n,k),couleur);((i+n,j,k-n),couleur)]@configuration) couleur case (n+1) 
    else configuration in
    [(case,couleur)]@(remplir_triangle_aux_1 configuration couleur case 1) in
  let (i,j,k)= case in
    if i < -dim then remplir_triangle_aux (remplir_triangle_1  configuration couleur (i+2 , j-1 , k-1)) couleur case 
    else configuration in
  let (liste1,liste2)=configuration in
    ((remplir_triangle_1 liste1  couleur  case ),([couleur]@liste2)) ;;

let rec remplir_init liste= 
  match liste with
    []-> ([],[])
    |head::tail-> remplir_triangle (tourne_config (remplir_init tail) )  head  ((-2* dim),dim,dim) ;;

let configuration_initial = remplir_init [Vert;Noir;Marron;Bleu;Rouge;Jaune];;

let est_dep_unit configuration case1 case2= 
  let (case_colorees,couleurs) = configuration in
   ((quelle_couleur case1 configuration)=List.hd(couleurs)) && ((quelle_couleur case2 configuration)=Libre) && (est_dans_losange case2) && (sont_cases_voisines case1 case2) 

    
let rec fait_dep_unit configuration case1 case2= 
  let (case_colorees,couleurs) = configuration in
    match case_colorees with
    []->configuration
    |head ::tail-> let ((case1_i,case1_j,case1_k),couleur),(case2_i,case2_j,case2_k)=(head,case1) in
        if case1_i=case2_i && case1_j=case2_j && case1_k=case2_k then   ((case2,couleur)::tail,couleurs)
        else let case_colorees2,couleurs2 = fait_dep_unit (tail,couleurs) case1 case2 in
        (head::case_colorees2,couleurs2) ;;

(*Mise à jour configuration 1ère partie*)

let mis_a_jour_configuration configuration coup= 
  match coup with 
    |Sm(l)-> Error ("Non traite, à faire dans la seconde partie") 
    |Du(coup1,coup2)-> if (est_dep_unit configuration coup1  coup2 ) then Ok (tourne_config (fait_dep_unit configuration coup1 coup2))
    else Error ("Ce n'est pas un déplacement unitaire");; 



let add_case a b = let (i,j,k) = a in let (i1,j1,k1) = b in (i+i1,j+j1,k+k1);;
      
let diff_case a b = let (i,j,k) = a in let (i1,j1,k1) = b in (i-i1,j-j1,k-k1);;

let calcul_pivot case1 case2 = let (i,j,k)= case1 in
  let (a,b,c) = case2 in
  if (a = i || b = j || c = k) then
    if((a+i) + (j+b) + (c+k) = 0) then
      Some ((i+a)/2, (j+b)/2, (k+c)/2)
    else None
  else None;;

  
let rec vec_aux v d = match v with
  (x,y,z) when x != 0 && x mod d = 0 -> ((x/d,y/d,z/d),d)
  |(x,y,z) when y != 0 && y mod d = 0 -> ((x/d,y/d,z/d),d)
  |(x,y,z) when z != 0 && z mod d = 0 -> ((x/d,y/d,z/d),d)
  |(x,y,z) -> vec_aux (x,y,z) (d-1) ;;

let vec_et_dist c1 c2 = vec_aux (diff_case c1 c2) 12;;


let case_moved vec case=
  let (v1, v2, v3) = vec in 
    let (c1,c2,c3) = case in 
      (c1-v1,c2-v2,c3-v3)
;;

let rec est_libre_aux vec pas conf case1 = 
  if pas = 0 then true 
  else if quelle_couleur case1 conf = Libre then true && est_libre_aux vec (pas-1) conf (case_moved vec case1) 
  else false
;;

let est_libre_seg case1 case2 conf = 
let (vec, pas) = vec_et_dist case1 case2 in 
  est_libre_aux vec pas conf case1
;;


let est_saut case1 case2 configuration =
  let res = calcul_pivot case1 case2 in 
    match res with 
    |None -> false 
    |Some(a,b,c) -> sont_cases_voisines case1 (a,b,c) && (quelle_couleur (a,b,c) configuration) <> Libre ;;  

let rec est_saut_mult caselist configuration =
match caselist with 
|[]-> false
|[x]->true  
|h::t -> est_saut h (List.hd t) configuration && est_saut_mult t configuration 
;; 

let mis_a_jour_configuration configuration coup= 
  match coup with 
    |Sm(l)-> if est_saut_mult l configuration then  Ok (tourne_config (fait_dep_unit configuration (List.hd l) (List.nth l (List.length l -1))))
    else Error ("pas saut")
    |Du(coup1,coup2)-> if (est_dep_unit configuration coup1  coup2 ) then Ok (tourne_config (fait_dep_unit configuration coup1 coup2))
    else Error ("Ce n'est pas un déplacement unitaire");; 

    (*Verification si victoire ou non*)

(*let rec parcours2 j i configuration current=
        if j > i then true 
        else if  current = quelle_couleur ((dim*2)-1+1, -dim-j+1, -dim+j-1) configuration then true && parcours2 (j+1) i configuration current
        else false

let rec parcours i configuration current =
  if i > dim then true
  else parcours2 1 i configuration current && parcours (i+1) configuration current
;;


let gagne configuration = 
  let current = List.hd @@ liste_joueurs configuration in 
  parcours 1 configuration current;;
*)
