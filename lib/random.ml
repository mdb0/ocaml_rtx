(*a huge part of the render is spend trying to generate random numbers with the standard Random lib so this is
  an effort to create a faster random number generator (even if the quality of the randomness is worst) *)

let seed = ref 0;;
let init_seed seed0 = seed := seed0;;

(*let hash() =     (*clearer but a bit slower than the next hash*)
    seed := !seed lxor 2747636419;
    seed := !seed * 2654435769;
    seed := !seed lxor (!seed lsr 16); 
    seed := !seed * 2654435769;
    seed := !seed lxor (!seed lsr 16); 
    seed := !seed * 2654435769;
;;*)

let hash() = (*hash with random bit operations*)
    let a = (!seed lxor 275674763756419) * 265644456435769 in
    let b = (a lxor (a lsr 16)) * 265443565235769 in
    seed := (b lxor (b lsr 16)) * 265443359857269;
;;

let random() =
    hash();
    abs_float(float_of_int(!seed))/.(float_of_int max_int);;



let randomLCG() = (*with a linear congruential generator*)
	let s = !seed in
	seed := (25214903917* s + 11) mod 281474976710656;
	abs_float(float_of_int s) *. 3.5527136788005009e-015;;

