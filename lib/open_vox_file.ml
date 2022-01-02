(*mdb*)

(*
open a .vox file and return (octree, palette, materials, cameras, obj_data, octree_size)
vox file structure:
	https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox.txt
	https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox-extension.txt
	


/!\ this loader doesn't support multiple objects in the scene and thus also layers and object rotations or translations
*)

(*//all types /////////////////////////////////////////////////////////////////////////////////////////////*)

type material = {
	mutable _type : string;
	mutable _weight : float;
	mutable _rough : float;
	mutable _spec : float;
	mutable _ior : float;
	mutable _att : float;
	mutable _flux : float;
	mutable _d : float;
	mutable _emit : float;
	mutable _metal : float;
};;

type camera = {
	  mutable _mode : string;
	  mutable _focus : float*float*float;
	  mutable _angle : float*float;
	  mutable _radius : float;
	  mutable _fov : float;
};;

type color = {r : float; g : float; b: float; a: float;};;

type voxel = {px: int; py: int; pz: int; colorID: int};;

type node = Node of node array | Leaf of voxel option;;

type object_data = {
	(*_bounce*)
	mutable _bounce_diffuse : int;
	mutable _bounce_specular : int;
	(*sky mode*)
	mutable _env_mode : int; (*0, 1, 2 for differents skylight *)
	(*environement 1*)
	mutable _inf_i : float;
	mutable _inf_k : float*float*float;
	mutable _inf_angle : float*float;
	mutable _inf_area : float;
	mutable _inf_disk : bool;
	(*env3*)
	mutable _atm_ray_d : float;
	mutable _atm_ray_k : float*float*float;
    mutable _atm_mie_d : float;
    mutable _atm_mie_k : float*float*float;
    mutable _atm_mie_g : float;
    mutable _atm_o3_d : float;
    mutable _atm_o3_k : float*float*float;
	(*sky color*)
	mutable _uni_i : float;
	mutable _uni_k : float*float*float;
	(*fog*)
	mutable _fog_d : float;
	mutable _fog_k : float*float*float;
	mutable _fog_g : float;
	(*camera*)
	mutable _lens_fov : float;
	mutable _lens_aperture : float;
	mutable _lens_blade_n : float;
	mutable _lens_blade_r : float;
	(*film*)
	mutable _film_expo : float;
	mutable _film_vig : float;
	mutable _film_gam : float;
	mutable _film_aces : bool;
	(*bloom*)
	mutable _bloom_mix : float;
	mutable _bloom_scale : float;
	mutable _bloom_aspect: float;
	mutable _bloom_threshold : float;
	(*ground*)
	mutable _ground_color : float*float*float;
	mutable _ground_hor : float;
};;


(*//functions to read things in a file//////////////////////////////////////////////////////////////////////*)

let skip f n = seek_in f ((pos_in f) + n);;

let input_int32 f = 256*256*256*(input_byte f)
					+ 256*256*(input_byte f)
					+ 256*(input_byte f)
					+ (input_byte f);;

let read_string f = really_input_string f (input_int32 f);;

let triple_float_of_string str = Scanf.sscanf str "%f %f %f" (fun x y z -> (x, z, y));;
let color_of_string str = Scanf.sscanf str "%f %f %f" (fun r g b -> ((r/.255.0), (g/.255.0), (b/.255.0)));;

(*//read voxel file/////////////////////////////////////////////////////////////////////////////////////////*)
let load_voxel_file path = (*main data arrays and constants from the file*)
	(*1: init all arrays to default values*)
	let palette = Array.map (fun x->{r=0.0; g=0.0; b=0.0; a=0.0;}) (Array.make 256 0) in
	let materials = Array.map (fun x -> {
						_type = "_diffuse"; _weight = 0.0;
						_rough = 0.2; _spec = 0.0;
						_ior = 1.3; _att = 0.0;
						_flux = 0.0; _d = 0.0;
						_emit = 0.0; _metal = 0.0;
					})(Array.make 256 0) in
	let cameras = Array.map (fun x -> { _mode = "pers";
						_focus = (0.0, 0.0, 0.0);
						_angle = (6.0, 0.0);
						_radius = 10.0; _fov = 60.0;
				}) ((Array.make 10 0)) in
	let obj_data = {
		_bounce_diffuse = 3;
		_bounce_specular = 2;
		_env_mode = 0;
		_inf_i = 1.0;
		_inf_k = (1.0, 1.0, 1.0);
		_inf_angle = (0.0, 0.0);
		_inf_area = 1.0;
		_inf_disk = false;
		_atm_ray_d = 1.0;
		_atm_ray_k = (1.0, 1.0, 1.0);
		_atm_mie_d = 1.0;
		_atm_mie_k = (1.0, 1.0, 1.0);
		_atm_mie_g = 1.0;
		_atm_o3_d = 1.0;
		_atm_o3_k = (1.0, 1.0, 1.0);
		_uni_i = 1.0;
		_uni_k = (1.0, 1.0, 1.0);
		_fog_d = 1.0;
		_fog_k = (1.0, 1.0, 1.0);
		_fog_g = 1.0;
		_lens_fov = 1.0;
		_lens_aperture = 1.0;
		_lens_blade_n = 1.0;
		_lens_blade_r = 1.0;
		_film_expo = 1.0;
		_film_vig = 0.1;
		_film_gam = 2.2;
		_film_aces = true;
		_bloom_mix = 0.5;
		_bloom_scale = 1.0;
		_bloom_aspect = 1.0;
		_bloom_threshold = 1.0;
		_ground_color = (1.0, 1.0, 1.0);
		_ground_hor = 0.0;
	} in
	let size_X = ref 0 in
	let size_Y = ref 0 in
	let size_Z = ref 0 in
	let voxels = ref [||] in
	
	(*2:read the file*)
	let f = open_in_bin path in (*open in binary mode to avoid translations made by open_in*)
	(try 
		seek_in f 20; (*pass les premier bytes pour aller directement dans le chunck MAIN*)
		while true do
			match really_input_string f 4 with
				|"SIZE" -> begin (*size in voxels uf the model*)
							   skip f 8; (*the size of this chunk is already known*)
							   size_X := input_int32 f;
							   size_Z := input_int32 f;
							   size_Y := input_int32 f;
							   
						   end
				|"XYZI" -> begin (*all voxels*)
								skip f 8; (*the size of this chunk is already known*)
								let num_voxels = input_int32 f in
								voxels:= Array.init num_voxels (fun x->
											let x = input_byte f in
											let y = input_byte f in
											let z = input_byte f in
											let color = input_byte f in 
											{px=x; py=z; pz=y; colorID= color-1}
										);
						   end
				|"RGBA" -> begin (*palette*)
							   skip f 8; (*the size of this chunk is already known*)
							   for i=0 to 255 do
									let r = (float_of_int (input_byte f) /.255.0) in
									let g = (float_of_int (input_byte f) /.255.0) in
									let b = (float_of_int (input_byte f) /.255.0) in
									let a = (float_of_int (input_byte f) /.255.0) in
									palette.(i) <- {r=r; g=g; b=b; a=a}
							   done  
						   end 
				|"MATL" -> begin (*materials*)
							  skip f 8;
							   let id = max 0 ((input_int32 f) - 1) in
							   let num_value = input_int32 f in
							   for i=1 to num_value do
								   let name = read_string f in
								   match name with
									   |"_type" -> materials.(id)._type <- read_string f
									   |"_weight" -> materials.(id)._weight <- float_of_string (read_string f)
									   |"_rough" -> materials.(id)._rough <- float_of_string (read_string f)
									   |"_spec" |"_sp" -> materials.(id)._spec <- float_of_string (read_string f)
									   |"_ior" -> materials.(id)._ior <- 1.0 +. float_of_string (read_string f)
									   |"_att" -> materials.(id)._att <- float_of_string (read_string f)
									   |"_flux" -> materials.(id)._flux <- 1.0 +. float_of_string (read_string f)
									   |"_d" -> materials.(id)._d <- float_of_string (read_string f)
									   |"_emit" -> materials.(id)._emit <- float_of_string (read_string f)
									   |"_metal" -> materials.(id)._metal <- float_of_string (read_string f)
									   |_ -> skip f (input_int32 f);
							   done;
						   end
				|"rCAM" -> begin (*cameras*)
								skip f 8;
								let id = input_int32 f in
								let num_value = input_int32 f in
								for i=1 to num_value do
								   let name = read_string f in
								   match name with
									   |"_mode" -> cameras.(id)._mode <- read_string f
									   |"_focus" -> cameras.(id)._focus <- triple_float_of_string (read_string f)
									   |"_angle" -> cameras.(id)._angle <- Scanf.sscanf (read_string f) "%f %f %f" (fun a b n -> (a/.180.0*.Float.pi, b/.180.0*.Float.pi))
									   |"_radius" -> cameras.(id)._radius <- float_of_string (read_string f)
									   |"_fov" -> cameras.(id)._fov <- float_of_string (read_string f)
									   |_ -> skip f (input_int32 f);
								done;
						   end
				|"rOBJ" -> begin (*word data like sun light, camera settings, bloom, ...*)
						    skip f 8;
						    let num_value = input_int32 f in
						    skip f (input_int32 f);
						    let mode = read_string f in
						    for i=2 to num_value do
							   let name = read_string f in
							   match name with
									|"_diffuse" -> obj_data._bounce_diffuse <- int_of_string(read_string f)
									|"_specular" -> obj_data._bounce_specular <- int_of_string(read_string f)
									|"_mode" -> obj_data._env_mode <- int_of_string(read_string f)
									|"_i" -> (match mode with
												|"_inf" -> obj_data._inf_i <- float_of_string(read_string f)
												|"_uni" -> obj_data._uni_i <- float_of_string(read_string f)
												|_-> skip f (input_int32 f))
									|"_k" -> (match mode with
												|"_inf" -> obj_data._inf_k <- color_of_string(read_string f)
												|"_uni" -> obj_data._uni_k <- color_of_string(read_string f)
												|"_fog_uni" -> obj_data._fog_k <- color_of_string(read_string f)
												|_-> skip f (input_int32 f))
									|"_angle" -> obj_data._inf_angle <- Scanf.sscanf (read_string f) "%f %f" (fun a b -> (a/.180.0*.Float.pi, b/.180.0*.Float.pi))
									|"_area" -> obj_data._inf_area <- max 0.003 (float_of_string(read_string f))
									|"_disk" -> obj_data._inf_disk <- (read_string f) = "1"
									|"_ray_d" -> obj_data._atm_ray_d <- float_of_string(read_string f)
									|"_ray_k" -> obj_data._atm_ray_k <- color_of_string(read_string f)
									|"_mie_d" -> obj_data._atm_mie_d <- float_of_string(read_string f)
									|"_mie_k" -> obj_data._atm_mie_k <- color_of_string(read_string f)
									|"_mie_g" -> obj_data._atm_mie_g <- float_of_string(read_string f)
									|"_o3_d" -> obj_data._atm_o3_d <- float_of_string(read_string f)
									|"_o3_k" -> obj_data._atm_o3_k <- color_of_string(read_string f)
									|"_d" -> obj_data._fog_d <- float_of_string(read_string f)
									|"_g" -> obj_data._fog_g <- float_of_string(read_string f)
									|"_fov" -> obj_data._lens_fov <- float_of_string(read_string f)
									|"_aperture" -> obj_data._lens_aperture <- float_of_string(read_string f)
									|"_blade_n" -> obj_data._lens_blade_n <- float_of_string(read_string f)
									|"_blade_r" -> obj_data._lens_blade_r <- float_of_string(read_string f)
									|"_expo" -> obj_data._film_expo <- float_of_string(read_string f)
									|"_vig" -> obj_data._film_vig <- float_of_string(read_string f)
									|"_aces" -> obj_data._film_aces <- (read_string f) = "1"
									|"_gam" -> obj_data._film_gam <- float_of_string(read_string f)
									|"_mix" -> obj_data._bloom_mix <- float_of_string(read_string f)
									|"_scale" when mode = "_bloom" -> obj_data._bloom_scale <- float_of_string(read_string f)
									|"_aspect" -> obj_data._bloom_aspect <- float_of_string(read_string f)
									|"_threshold" -> obj_data._bloom_threshold <- float_of_string(read_string f)
									|"_color" when mode = "_ground" -> obj_data._ground_color <- color_of_string(read_string f)
									|"_hor" -> obj_data._ground_hor <- float_of_string(read_string f)
									|_-> skip f (input_int32 f)
							done;
						   end
				|s-> let chunk_content = input_int32 f in (*unused chunk*)
					 let chunk_children = input_int32 f in
					 begin
						print_string "unread chunk: ";
						print_endline s;
						skip f (chunk_content + chunk_children) (*skip this chunk that in not supported by our implementation*)
					 end
		done
	with 
		|End_of_file -> close_in f
		|e -> raise e);
	(!voxels, palette, materials, cameras, obj_data, !size_X, !size_Y, !size_Z);;


(*//create the octree///////////////////////////////////////////////////////////////////////////////*)  


let append_tree tree voxel size = (*add a voxel in the octree*)
    let rec append_tree_rec tree voxel size x0 y0 z0 =
		match tree with
			|Leaf _ when size = 1 -> Leaf (Some voxel)
			|_ -> begin
				let children = match tree with
								|Leaf _ -> Array.init 8 (fun x->Leaf None)
								|Node child -> child in
				
				let next_size = size/2 in
				let x1 = ref x0 in
				let y1 = ref y0 in
				let z1 = ref z0 in
				let idx = ref 0 in
				if voxel.px - x0 > next_size then begin idx := !idx + 4;
														x1 := x0 + next_size end;
				if voxel.py - y0 > next_size then begin idx := !idx + 2;
														y1 := y0 + next_size end;
				if voxel.pz - z0 > next_size then begin idx := !idx + 1;
														z1 := z0 + next_size end;
				children.(!idx) <- append_tree_rec children.(!idx) voxel next_size !x1 !y1 !z1;
				
				Node children 
			end
	in append_tree_rec tree voxel size 0 0 0;;


let createOctree voxels size_X size_Y size_Z = 
	(*get octree size (smallest power of 2 bigger than the voxel model)*)
	let max_dim = max (max size_X size_Y) size_Z in
	let octree_size = let dimPow2_tmp = ref 1 in
							while !dimPow2_tmp < max_dim do dimPow2_tmp := !dimPow2_tmp * 2;
							done;
							!dimPow2_tmp in
	let octree = ref (Leaf None) in
	
	let offset_X = octree_size - size_X in (*to center the model in the octree*)
	let offset_Y = 0 in
	let offset_Z = octree_size - size_Z in
	
	Array.iter (fun vox -> (*insert all voxels into the octree*)
		octree := append_tree !octree {px = vox.px + offset_X/2;
									   py = vox.py + offset_Y/2;
									   pz = vox.pz + offset_Z/2;
									   colorID = vox.colorID
									} octree_size
				) voxels;
	(!octree, octree_size);;

(*//main function////////////////////////////////////////////////////////////*)

let load_octree path = 
	let (voxels, palette, materials, cameras, obj_data, size_X, size_Y, size_Z) = load_voxel_file path in
	let octree, octree_size = createOctree voxels size_X size_Y size_Z in
	(octree, palette, materials, cameras, obj_data, octree_size);;

















