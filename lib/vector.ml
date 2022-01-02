(*mdb*)

(*vector type with basique operations, all vector operations use ':' and '.' means it's a float that follow*)
type vec3 = {mutable x: float; mutable y:float; mutable z:float};;

let vec3 (a, b, c) = {x=a; y=b; z=c};;
(*operation vector / vector: *)
let ( +: ) a b = {x=a.x+.b.x; y=a.y+.b.y; z=a.z+.b.z};;
let ( -: ) a b = {x=a.x-.b.x; y=a.y-.b.y; z=a.z-.b.z};;
let ( *: ) a b = {x=a.x*.b.x; y=a.y*.b.y; z=a.z*.b.z};;
let ( /: ) a b = {x=a.x/.b.x; y=a.y/.b.y; z=a.z/.b.z};;
(*operation vector / float: *)
let ( +:. ) v k = {x=v.x+.k; y=v.y+.k; z=v.z+.k};;
let ( -:. ) v k = {x=v.x-.k; y=v.y-.k; z=v.z-.k};;
let ( *:. ) v k = {x=v.x*.k; y=v.y*.k; z=v.z*.k};;
let ( /:. ) v k = {x=v.x/.k; y=v.y/.k; z=v.z/.k};;
let ( **:. ) v k = {x=v.x**k; y=v.y**k; z=v.z**k};;
(*operation ref vector / vector: *)
let ( +:= ) a b = a := !a +: b;;
let ( -:= ) a b = a := !a -: b;;
let ( *:= ) a b = a := !a *: b;;
let ( /:= ) a b = a := !a /: b;;
(*operation ref vector / float: *)
let ( +:.= ) v k = v := !v +:. k;;
let ( -:.= ) v k = v := !v -:. k;;
let ( *:.= ) v k = v := !v *:. k;;
let ( /:.= ) v k = v := !v /:. k;;
let ( **:.= ) v k = v := !v **:. k;;
(*map a function to all components of a vector: *)
let mapv foo v = {x=foo v.x; y=foo v.y; z=foo v.z};;
let expv v = mapv exp v;;
let sinv v = mapv sin v;;
let cosv v = mapv cos v;;
(*others: *)
let dot a b = a.x*.b.x +. a.y*.b.y +. a.z*.b.z;;
let norm v = sqrt(dot v v);;
let normalize v = v /:. (norm v);;

let cross v1 v2 = 
	{x=v1.y *. v2.z -. v1.z *. v2.y;
	 y=v1.z *. v2.x -. v1.x *. v2.z;
	 z=v1.x *. v2.y -. v1.y *. v2.x;
	};;

let reflect v n = v -: (n *:. (2.0 *. (dot n v)));;
(*create a copy of a vector: *)
let copy v = {x=v.x; y=v.y; z=v.z};;
(*print a vector: *)
let print_vec3 v = Printf.printf "vec3 %f, %f, %f" v.x v.y v.z;;
(*test of fast inverse sqrt for the normalize function but it's a bit slower then the existing sqrt*)
let fast_inv_sqrt x =
	let y  = Int64.float_of_bits(Int64.sub (0x5fe6eb50c7b537a9L) (Int64.shift_right (Int64.bits_of_float x) 1)) in
	y *. (1.5 -. (x *. 0.5 *. y *. y));;


