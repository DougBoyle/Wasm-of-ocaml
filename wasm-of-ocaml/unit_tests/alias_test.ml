(* Regression test for handling of aliases *)
let make_perms =
  let rec random_perm ((p_f, p_v) as acc) i =
    random_perm acc (i - 1) in
  random_perm ([], [])

