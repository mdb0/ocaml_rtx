#use "./lib/vector.ml";;
#use "./lib/random.ml";;
#use "./lib/open_vox_file.ml";;

#load "graphics.cma";;
open Graphics;;

(*//user inputs///////////////////////////////////////////////////////////////////////////////////////////////*);;




let adaptive_sampling = false;;  (*really good for models with a lot of space with low detail(ex: metal2.vox) or when used with denoising or for big resolutions*)
let path = "./models/chr_knight.vox";;




(*toggle denoising with the space bar (slow)*)
(*change depth of field with mouse clic*)

(*loading bar colors:
	-yellow: ray tracing
	-green: denoising
	-blue: color correction
*)

(*advanced settings (that are not in the model file)*)
let bloom_SPP = 1;;       (*number of sample used for bloom at each frame*);;
let denoising_depth = 2;; (*the bigger, the more heavy the denoising will get, make it bigger (up to 3 or 4) for bigger image size *);;
let sigma_rt = 4.0;;   (*denoising color difference factor*)
let sigma_n = 0.5;;    (*denoising normal difference factor*)
let sigma_x = 0.001;;  (*denoising position difference factor*)
let adaptive_sampling_treshold = 0.01;;  (*minimal standard deviation of the pixel region*)

(*////////////////////////////////////////////////////////////////////////////////////////////////////////////*);;
(*////////////////////////////////////////////////////////////////////////////////////////////////////////////*);;
(*////////////////////////////////////////////////////////////////////////////////////////////////////////////*);;
(*load the octree, a lot of functions after need the octree to be loaded before compiling*)

let octree, palette, materials, cameras, world_info, octree_size = load_octree path;;

let diffuse_bounces = world_info._bounce_diffuse;;
let specular_bounces =  world_info._bounce_specular;;

(*//raytracing functions//////////////////////////////////////////////////////////////////////////*);;

let sign x = if x < 0.0 then -1.0 else 1.0;; (*zero is not supposed to enter here*)

(*# octree traversal #  modified from https://lsi2.ugr.es/curena/inves/wscg00/revelles-wscg00.pdf *)
let find_firstNode t0 tM =
    let first = ref 0 in
    if t0.x > t0.y && t0.x > t0.z then begin (* entry XY*)
        if tM.y < t0.x then first := !first + 2;
        if tM.z < t0.x then first := !first + 1;
    end 
    else 
		if t0.y > t0.z then begin (* entry XZ*)
			if tM.x < t0.y then  first := !first + 4;
			if tM.z < t0.y then first := !first + 1;
		 end else begin  				 (* entry YZ*)
			if tM.x < t0.z then first := !first + 4;
			if tM.y < t0.z then first := !first + 2;
		end;
    !first;;


let next_node t1 yz xz xy = (*output next visited node given all exit sides*)
    if t1.x < t1.y && t1.x < t1.z then
        yz
    else if t1.y < t1.z then
        xz
    else
        xy;;

let rec proc_tree t0 t1 node a = (*main octree traversal*)
	if t1.x < 0.0 || t1.y < 0.0 || t1.z < 0.0 then (*behind the camera*)
		None
	else begin
	match node with
		|Leaf None -> None     (*↓ distance, voxel, normal*)
		|Leaf Some vox -> Some (if(t0.x > t0.y && t0.x > t0.z) then  (* entry YZ *)
								(t0.x, vox, vec3(1.0, 0.0, 0.0))
							  else if(t0.y > t0.z) then 			(* entry XZ *)
								(t0.y, vox, vec3(0.0, 1.0, 0.0))
							  else									(* entry XY *)
								(t0.z, vox, vec3(0.0, 0.0, 1.0)))
		|Node children -> 
	
	let tm = (t0 +: t1) *:. 0.5 in
	let currNode = ref (find_firstNode t0 tm) in
	let result = ref None in
	while !currNode < 8 && !result = None do
		match !currNode with
			|0 -> begin result := proc_tree t0 tm children.(a) a;
						currNode := next_node tm 4 2 1 end
						
			|1 -> begin result := proc_tree (vec3(t0.x, t0.y, tm.z)) (vec3(tm.x, tm.y, t1.z)) children.(1 lxor a) a;
						currNode := next_node (vec3(tm.x, tm.y, t1.z)) 5 3 8 end
						
			|2 -> begin result := proc_tree (vec3(t0.x, tm.y, t0.z)) (vec3(tm.x, t1.y, tm.z)) children.(2 lxor a) a;
						currNode := next_node (vec3(tm.x, t1.y, tm.z)) 6 8 3 end
						
			|3 -> begin result := proc_tree (vec3(t0.x, tm.y, tm.z)) (vec3(tm.x, t1.y, t1.z)) children.(3 lxor a) a;
						currNode := next_node (vec3(tm.x, t1.y, t1.z)) 7 8 8 end
						
			|4 -> begin result := proc_tree (vec3(tm.x, t0.y, t0.z)) (vec3(t1.x, tm.y, tm.z)) children.(4 lxor a) a;
						currNode := next_node (vec3(t1.x, tm.y, tm.z)) 8 6 5 end
						
			|5 -> begin result := proc_tree (vec3(tm.x, t0.y, tm.z)) (vec3(t1.x, tm.y, t1.z)) children.(5 lxor a) a;
						currNode := next_node (vec3(t1.x, tm.y, t1.z)) 8 7 8 end
						
			|6 -> begin result := proc_tree (vec3(tm.x, tm.y, t0.z)) (vec3(t1.x, t1.y, tm.z)) children.(6 lxor a) a;
						currNode := next_node (vec3(t1.x, t1.y, tm.z)) 8 8 7 end
						
			|7 -> begin result := proc_tree tm t1 children.(7 lxor a) a;
						currNode := 8 end
	done;
	!result 
	end;;


let ray_step ro rd = (*pre prossed the ray before the octree traversal*)
	let ro2 = copy ro in
	let rd2 = copy rd in
    let a = ref 0 in
    let flip = vec3(-.(sign rd.x), -.(sign rd.y), -.(sign rd.z)) in
    ro2.y <- ro2.y -. 0.5; (*center the octree on (0.0, 0.5, 0.0) and the octree size is 1.0*)

    if (rd2.x < 0.0) then begin (*flip ray on X axis*)
        ro2.x <- -.ro2.x;
        rd2.x <- -.rd2.x;
        a := !a + 4;
    end;
    if (rd2.y < 0.0) then begin (*flip ray on Y axis*)
        ro2.y <- -.ro2.y;
        rd2.y <- -.rd2.y;
        a := !a + 2;
    end;
    if (rd2.z < 0.0) then begin (*flip ray on Z axis*)
        ro2.z <- -.ro2.z;
        rd2.z <- -.rd2.z;
        a := !a + 1;
    end;
    (*ray cube intersection*)
    let invDir = (vec3(-1.0, -1.0, -1.0))/:rd2 in
    let t0 = (ro2 +:. 0.5) *: invDir in
    let t1 = (ro2 -:. 0.5) *: invDir in

    let tmin = max(max t0.x t0.y) t0.z in
    let tmax = min(min t1.x t1.y) t1.z in
    if ((tmin < tmax) && (tmax > 0.0)) then begin (*intersect the octree*)
        match proc_tree t0 t1 octree !a with
			|None -> None
			|Some (t, vox, normal) ->Some (t, vox, normal *: flip)
		end
    else (*doesn't intersect the octree*)
        None;;



(*# create sun constantes #*)
let phi_sun, teta_sun = world_info._inf_angle
let sun_dir = vec3((cos phi_sun)*.(sin teta_sun), (*direction of the sun light*)
					(sin phi_sun), 
				-.(cos phi_sun)*.(cos teta_sun))

let area = world_info._inf_area**2.0 *. Float.pi

let sun_color = (vec3 world_info._inf_k **:.2.2 ) *:. (world_info._inf_i *. 44.0 /. area)

let sun_cos = sqrt(1.0 -. world_info._inf_area**2.0 *. 0.3) (*cos of the angle size of the sun in the sky*)
let sun_prob = area /. (4.0 *. Float.pi) (*sun weight for importance sampling*)
;;

(*# get main color from scene #*)
let out = 1e50;;
let get_scene ro rd isCameraRay = 
	let t = ref out in
	let col = ref ((vec3 world_info._uni_k**:.2.2)*:.(world_info._uni_i*.1.5)) in (*sky color*)
	if isCameraRay && world_info._inf_disk && (dot rd sun_dir > sun_cos) then
		col := sun_color;
	let id = ref (0) in
	let normal = ref (vec3(0.0, 1.0, 0.0)) in
	
	let d_plan = (if rd.y <> 0.0 then -.ro.y /. rd.y else out) in
	
	if d_plan > 0.0 && d_plan < !t then begin
		t := d_plan;
		col := vec3 world_info._ground_color;
	end;
	
	(match ray_step ro rd with
		|Some (t_model, vox, normal2) when t_model < !t -> let color = palette.(vox.colorID) in begin
											let r = color.r in
											let g = color.g in
											let b = color.b in
											col := vec3(r, g, b);
											normal := normal2;
											t := t_model;
											id := vox.colorID;
									  end
		|_ -> ());
	(!t, !normal, !col, !id);;

(*BSDF and importance sampling /////////////////////////////////////////*)

let lerp a b t = a +. (b -. a)*.t;;(*linear interpolation*)

let createBasis normal = (*return a direct orthogonal basis with normal as the Y vector*)
    let tangent = normalize (cross normal (vec3(1.0, 0.0, 1.0))) in
    let binormal = cross normal tangent in
    (tangent, normal, binormal);;



let sphericalDirection sinTheta cosTheta sinPhi cosPhi = (*unit vector from spherical coordinate*)
    vec3(sinTheta *. cosPhi, cosTheta, sinTheta *. sinPhi);;

let uniformSampleCone cosThetaMax xbasis ybasis zbasis = (*random unit vector within a cone*)
	let cosTheta = lerp 1.0 cosThetaMax (random()) in
    let sinTheta = sqrt(1. -. cosTheta *. cosTheta) in
    let phi = (random()) *. 2.0 *. Float.pi in
    let samplev = sphericalDirection sinTheta cosTheta  (sin phi) (cos phi) in
     (xbasis *:. samplev.x) +: (ybasis *:. samplev.y) +: (zbasis *:. samplev.z);;



let cosineSampleHemisphere xbasis ybasis zbasis = (*random unit vector(wi) in an hemisphere with a probability proportionnal to (cos wi, y_axis)*)
    let r0 = random() in
    let r = sqrt r0 in
    let theta = 2.0 *. Float.pi *. random() in
    (xbasis *:.(r *. (sin theta))) +: (ybasis *:. (sqrt(1.0 -. r0))) +:(zbasis*:. (r *. (cos theta)));;


(*BSDF and PDF, 
here I've assume that my PDFs for importance sampling are made close enouph to the BSDF that 
they both cancel each other in the rendering equation (except for sun light rays)
rendering equation used: (with "L(position, direction)" the main function)
																						
L(ro, rd) = e(ro + t*rd) + ∫ BSDF(wi) / pdf(wi) * L(ro + t*rd, wi) * dot(n, wi) d wi	
						   Ω															

ro: ray origin
rd: ray direction
t: intersection distance
e: emission
BSDF: bidirectional scattering distribution function
pdf: probability density function
n: surface normal
Ω: hemisphere
*);;

let diffuseSample rd normal id = (*BSDF = constant*)
	let xbasis, ybasis, zbasis = createBasis normal in
	cosineSampleHemisphere xbasis ybasis zbasis;;

let microfacetSample rd normal id = (*materials with reflections, BSDF: more values toward reflected vector*)
	let phi = 2.0 *. Float.pi *. random() in
	
    let alpha = materials.(id)._rough**4.0 in
    let rnd = random() in
    let cosTheta = sqrt(rnd /. (lerp rnd 1.0 alpha)) in
    let sinTheta = sqrt(1.0 -. cosTheta *. cosTheta) in
    let whLocal = sphericalDirection sinTheta cosTheta (sin phi) (cos phi) in (*wh = normalize(-rd + rd') with rd' the new vector*)
     
    let xbasis, ybasis, zbasis = createBasis normal in
    let wh = (xbasis*:.whLocal.x) +: (ybasis *:. whLocal.y) +: (zbasis *:. whLocal.z) in
    
    reflect rd wh;;

let sunSample ro normal mask = (*direct light sample toward the sun*)
	if(dot normal sun_dir < 0.0 || world_info._inf_i < 0.01) then 
		vec3(0.0, 0.0, 0.0)
	else begin
		let xbasis, ybasis, zbasis = createBasis sun_dir in
		let rd = uniformSampleCone sun_cos xbasis ybasis zbasis in
		let _DdotN = dot rd normal in
		if  _DdotN < 0.0 then
			vec3(0.0, 0.0, 0.0)
		else begin
			let t, _,col,_ = get_scene ro rd (false) in
			if t >= out then
				mask *: sun_color *:. (sun_prob *. _DdotN) (*muliply by sun_prob, the pdf*)
			else
				vec3(0.0, 0.0, 0.0)
		end
	end;;


(*//main fragment shader //////////////////////////////////////////////////////////////////////////////////*)

(*compute some constants*)
let fov_resize = tan((world_info._lens_fov /. 360.0) *. Float.pi)
let _DOF = ref (-1.0) (*-1.O means no dof (Depth Of Field)*)
let phi, teta =  cameras.(0)._angle
let target = (vec3 cameras.(0)._focus) /:. (float_of_int octree_size)
let camPos = target+:((vec3((cos phi)*.(sin teta), 
						 -.(sin phi), 
						 -.(cos phi)*.(cos teta)))*:. (cameras.(0)._radius/. (float_of_int octree_size)))


let dir0 = normalize(target -: camPos)
let up = vec3(0.0, 1.0, 0.0) 
let right = normalize(cross up dir0)
let up = cross dir0 right 


let rec march ro rd mask diffuse specular isCameraRay ref_normal ref_base ref_pos= (*recursive evaluation of the rendering equation with Monte-Carlo integration*)
		if dot mask mask < 0.0001 then
			vec3(0.0, 0.0, 0.0)
		else begin
			let t, normal, col, id = get_scene ro rd isCameraRay in
			if t >= out then begin(*dont collide with the scene*)
				if isCameraRay then ref_base:= col;
				mask *: col
			end else begin
				let pos = ro +: (rd *:. t) in
				if isCameraRay then begin ref_normal:= normal; ref_base:= col; ref_pos:= pos end;
				match materials.(id)._type with
					|"_emit" -> begin (*emissive material*)
							(mask *: col) *:. ((exp (materials.(id)._flux *. 1.5)) *. materials.(id)._emit)
							end
					|"_metal" -> begin (*diffuse and reflective material (micro facet)*)
								(if specular > 0 then 
									march (pos +: (normal*:.0.01)) (microfacetSample rd normal id) (mask *: col) (diffuse-1) (specular-1) (false) (ref_normal) (ref_base) (ref_pos)
								else
									vec3(0.0, 0.0, 0.0))
								+:(if diffuse > 0 && materials.(id)._metal < 0.95 then 
									((march (pos +: (normal*:.0.01)) (diffuseSample rd normal id) (mask *: col) (diffuse-1) (specular-1) (false)) (ref_normal) (ref_base) (ref_pos)
									+: (sunSample (pos +: (normal*:.0.01)) (normal) (mask *: col))
									) *:. 0.5
								else
									vec3(0.0, 0.0, 0.0)
								)
								end
					|"diffuse" |_ -> begin (*diffuse mat material*)
								if diffuse > 0 then 
									(march (pos +: (normal*:.0.01)) (diffuseSample rd normal id) (mask *: col) (diffuse-1) (specular-1) (false)) (ref_normal) (ref_base) (ref_pos)
									+: (sunSample (pos +: (normal*:.0.01)) (normal) (mask *: col))
								else
									vec3(0.0, 0.0, 0.0)
								end;
				 end;
			end;;

let frag fragCoord resolution = (*main fragment shader*)
	let uv = (((fragCoord +: (vec3(random(), random(), 0.0))) /: resolution)*:. 2.0) -:. 1.0 in (*add randomness for antialiasing*)
	uv.x <- uv.x *. resolution.z *. fov_resize;
	uv.y <- uv.y *. fov_resize;
	let rd = normalize(dir0 +: (right *:. uv.x) +: (up *:. uv.y)) in
	
	let normal = ref (vec3(0.0, 0.0, 0.0)) in
	let base = ref (vec3(0.0, 0.0, 0.0)) in
	let pos = ref (vec3(0.0, 0.0, 0.0)) in
	if !_DOF < 0.0 then (*no depth of field*)
		let col = march camPos rd (vec3(1.0, 1.0, 1.0)) diffuse_bounces specular_bounces true normal base pos in
		(col, !normal, !base, !pos)
	else begin (*depth of field*)
		let t = !_DOF /. (dot rd dir0) in (*distance to the focal plane*)
		let focalPoint = camPos +: (rd *:. t) in
		let teta = random() *. 2.0 *. Float.pi in
		let off2 = (vec3(cos teta, sin teta, 0.0)) *:. ((sqrt (random())) *. world_info._lens_aperture *. 0.03) in
		let camPos2 = camPos +: (right*:.off2.x) +: (up*:.off2.y) in
		let rd2 = normalize (focalPoint -: camPos2) in
		let col = march camPos2 rd2 (vec3(1.0, 1.0, 1.0)) diffuse_bounces specular_bounces true normal base pos in
		(col, !normal, !base, !pos);
	end;;

(*//denoising///////////////////////////////////////////////////////////////////////////////*);;
let clamp = (fun x -> max (min x 1.0) 0.0);;
let clamp2 x dim = min (dim-1) (max 0 x);;

let denoising = ref false;;
let aff_progress x h =
	fill_rect 0 (h-2) x 2;;

let denoise w h denoised_buffer post_process_buffer normal_buffer pos_buffer base_color_buffer = (*denoise the image with normal, position and color information*)
	if !denoising then begin  (*https://jo.dreggn.org/home/2010_atrous.pdf*)
		set_color 0x9ACD32;
		let kernel = [|1./.16.; 1./.4.; 3./.8.; 1./.4.; 1./.16.|] in
		for d = 1 to denoising_depth do
			let tmp1 = if d mod 2 = 1 then denoised_buffer else post_process_buffer in
			let tmp2 = if d mod 2 = 0 then denoised_buffer else post_process_buffer in
			let pas = 1 lsl (d-1) in
			for x=0 to w-1 do
			for y=0 to h-1 do
				let total = ref (vec3(0.0, 0.0, 0.0)) in
				let cum_w = ref 0.0 in
				let cval = tmp1.(y).(x) /: base_color_buffer.(y).(x) in (*dividing by base color to denoise only indirect lighting and keep sharp voxels*)
				let nval = normal_buffer.(y).(x) in
				let pval = pos_buffer.(y).(x) in
				let d = norm (pval) in
				
				for dx = -2 to 2 do (*loop in the 2D 5x5 kernel*)
				for dy = -2 to 2 do
					let dx2 = clamp2 (x + dx*pas) w in
					let dy2 = clamp2 (y + dy*pas) h in
					(*color difference weight*)
					let ctmp = tmp1.(dy2).(dx2) /: base_color_buffer.(dy2).(dx2) in
					let t1 = cval -: ctmp in
					let dist1 = dot t1 t1 in
					let c_w = exp (-.dist1/.sigma_rt) in
					(*normal difference weight*)
					let ntmp = normal_buffer.(dy2).(dx2) in
					let t2 = nval -: ntmp in
					let dist2 = dot t2 t2 in
					let n_w = exp (-.dist2/.sigma_n) in
					(**position difference weight*)
					let ptmp = pos_buffer.(dy2).(dx2) in
					let t3 = pval -: ptmp in
					let dist3 = dot t3 t3 in
					let p_w = exp(-.dist3/.(sigma_x*.(max 1.0 (d*.10.0)))) in
					(*total weight of the pixel*)
					let weight = c_w *. n_w *. p_w *. kernel.(dx + 2) *. kernel.(dy + 2) in
					total +:= ctmp *:. weight;
					cum_w := !cum_w +. weight;
				done done;
				tmp2.(y).(x) <- (!total *: base_color_buffer.(y).(x)) /:. !cum_w ;
			done;
			if x mod 4 = 0 then aff_progress (x/denoising_depth + (d-1)*w/denoising_depth) h; (*loading bar*)
		 done
		done;
	end;;
(*post prossesing/////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*bloom*)
let sigma = 0.02 +. 0.04 *. world_info._bloom_scale;; (*for bloom normal distribution*)
let pass_threshold col = (*keep light only abrove a threshold*)
	let len2 = dot col col in
	if len2 <  world_info._bloom_threshold**2.0 then 
		vec3(0.0, 0.0, 0.0)
	else if len2 < 100.0 then 
		col
	else
		col *:. (10.0 /. sqrt len2);;


let normal_distributed_values scale mu sigma = (*return two int with a normal distribution*)
	let u = random() in
	let v = random() in
	let mag = sigma *. sqrt (-2.0 *. log u) in
	let sizef = float_of_int (scale - 1) in
	let z0 = sizef *. clamp (mu +. mag *. cos (2.0 *. Float.pi *. v)) in
	let z1 = sizef *. clamp (mu +. mag *. sin (2.0 *. Float.pi *. v)) in
	(int_of_float (z0), int_of_float (z1));;

let compute_bloom w h factor frame_buffer bloom_first_pass bloom_second_pass = 
	(*compute bloom with monte carlo integration for vertical blur and then, a horizontal blur of the vertical blur*)
	if world_info._bloom_mix > 0.05 then begin
	(*first pass*)
	for y=0 to h-1 do
		for x=0 to w-1 do
			let col_accu = ref (vec3(0.0, 0.0, 0.0)) in
			for i=1 to bloom_SPP do
				let x0, x1 = normal_distributed_values w (float_of_int x /. float_of_int (w-1)) sigma in
				col_accu +:= pass_threshold frame_buffer.(y).(x0);
				col_accu +:= pass_threshold frame_buffer.(y).(x1);
			done;
			bloom_first_pass.(y).(x) <- (bloom_first_pass.(y).(x) *:. (1.0 -. factor))
									+: (!col_accu *:. (factor /. (float_of_int (bloom_SPP * 2))))
		done;
	done;
	(*second pass*)
	for y=0 to h-1 do
		for x=0 to w-1 do
			let col_accu = ref (vec3(0.0, 0.0, 0.0)) in
			for i=1 to bloom_SPP do
				let y0, y1 = normal_distributed_values h (float_of_int y /. float_of_int (h-1)) (sigma *. (1.0 -. world_info._bloom_aspect)) in
				col_accu +:= pass_threshold bloom_first_pass.(y0).(x);
				col_accu +:= pass_threshold bloom_first_pass.(y1).(x);
			done;
			bloom_second_pass.(y).(x) <- (bloom_second_pass.(y).(x) *:. (1.0 -. factor))
									+: (!col_accu *:. (factor /. (float_of_int (bloom_SPP * 2))))
		done;
	done;
	end;;

(*color corrections*)
let to_col255 col = (*color in range 0-1 float to 0-255 int*)
	let r = int_of_float (255.0*.(clamp col.x))
	and g = int_of_float (255.0*.(clamp col.y))
	and b = int_of_float (255.0*.(clamp col.z)) in
	rgb r g b;;

let aces = (fun x -> x*.(2.51*.x +. 0.03)/.(x*.(2.43*.x +. 0.59) +. 0.14));; (*partial ACES tone mapping (without mapping to the XYZ color space)*)
let basique = (fun x -> 1.0 -. exp (-2.0 *. x));; (*a simple hdr tone mapping*)

let post_process_frag x y w h in_color fragCoord resolution bloom_pass = (*post processing shader for tone mapping, exposure, gamma, bloom, vignette*)
	let uv = fragCoord/:resolution in
	let vig = 16.0 *. uv.x *. uv.y *. (1.0-.uv.x) *. (1.0-.uv.y) in
	
	let out_color = ref in_color in
	(*bloom*)
	if !denoising then
		let factor = world_info._bloom_mix *. 0.3 /. 9.0 in
		for dx = -1 to 1 do
		for dy = -1 to 1 do
			out_color +:= bloom_pass.(clamp2 (y+dy) h).(clamp2 (x+dx) w) *:. factor
		done
		done
	else
		out_color +:= (bloom_pass.(y).(x)) *:. (world_info._bloom_mix *. 0.3);
	(*tone mapping and gamma correction*)
	out_color *:.= world_info._film_expo;
	let tone_map = if world_info._film_aces then aces else basique in
					
	out_color := mapv tone_map !out_color;
	out_color *:.= (lerp 1.0 vig world_info._film_vig);
	out_color **:.= world_info._film_gam;
	!out_color;;

(*//function for user inputs/////////////////////////////////////////////*);;

exception Restart;;
let event_handler t0 resolution =
	if button_down() && Sys.time() -. t0 > 0.4 then begin (*test if user change the focus, if so, ray trace the intersection with the scene to know the new focal length (and use minimal time before update to avoid double-click)*)
		let x, y = mouse_pos() in
		let uv = (((vec3(float_of_int x, float_of_int y, 0.0)) /: resolution) *:. 2.0) -:. 1.0 in
		uv.x <- uv.x *. resolution.z *. fov_resize;
		uv.y <- uv.y *. fov_resize;
		let rd = normalize(dir0 +: (right *:. uv.x) +: (up *:. uv.y)) in
		let t, _, _, _ = get_scene camPos rd (true) in
		if t >= out then
			_DOF := -1.0
		else
			_DOF := t;
		print_endline "new focal length set";
		raise Restart;
	end;
	(*check if denoising have been toggle with the space bar*)
	while key_pressed() do
		match read_key() with
			|' '->begin denoising := not !denoising; Printf.printf "denoising: %b" !denoising; print_newline() end 
			|_->()
	done;;
(*//graphics functions////////////////////////////////////////////////////////////////////////////*);;
let init w h = (*min size 120*)
	open_graph ((string_of_int w)^"x"^(string_of_int h));;

let newframe w h accu frame_buffer image_array bloom_first_pass bloom_second_pass normal_buffer base_color_buffer pos_buffer denoised_buffer post_process_buffer is_active_buffer =
    let t0 = Sys.time () in
	let wf = float_of_int w in
	let hf = float_of_int h in
	let resolution = vec3(wf, hf, wf/.hf)in (*width, height, aspect ratio*)
	let factor = 1.0/.float_of_int(accu + 1) in (*accumulation factor for Monte Carlo integration*)
	(*1: first loop, main color information*)
	set_color 0xffd700;
	for x=0 to w-1 do
		for y=0 to h-1 do
			let fragCoord = vec3((float_of_int x), (float_of_int (h - 1 - y)), 0.0) in
			if is_active_buffer.(y).(x) then begin
				if  not adaptive_sampling ||accu = 0 || (let neighbors = Array.init 9 (fun i -> post_process_buffer.(clamp2 (y+(i/3 -1)) h).(clamp2 (x+(i mod 3 -1)) w)) in
												  let _E = (Array.fold_left (+:) (vec3(0.0, 0.0, 0.0)) neighbors) /:. 9.0 in
												  let _V = ((Array.fold_left (fun s x -> s +: (x**:.2.0)) (vec3(0.0, 0.0, 0.0)) neighbors)/:.9.0) -: (_E**:.2.0)  in
												  _V.x +. _V.y +. _V.z > adaptive_sampling_treshold*.adaptive_sampling_treshold
												  )
				then begin
					let col, normal, base, pos = frag fragCoord resolution in 		(*new color value*)
					frame_buffer.(y).(x) <- (col*:.factor) +: (frame_buffer.(y).(x) *:. (1.0 -. factor));(*average between all color values*)
					normal_buffer.(y).(x) <- (normal*:.factor) +: (normal_buffer.(y).(x) *:. (1.0 -. factor));(*average between all normals values (not constant because of DOF and antialiasing)*)
					pos_buffer.(y).(x) <- (pos*:.factor) +: (pos_buffer.(y).(x) *:. (1.0 -. factor));
					base_color_buffer.(y).(x) <- (base*:.factor) +: (base_color_buffer.(y).(x) *:. (1.0 -. factor));
					base_color_buffer.(y).(x) <- mapv (max 0.0001) base_color_buffer.(y).(x);
				end else begin 
					is_active_buffer.(y).(x) <- false;
				end;
			end;
			denoised_buffer.(y).(x) <- frame_buffer.(y).(x)
		done;
		(*loading bar*)
		if x mod 4 = 0 then aff_progress x h;
		(*check fo user inputs*)
		if x mod 4 = 0 then event_handler t0 resolution;
	done;
	(*2: second loop, bloom*)
	compute_bloom w h factor frame_buffer bloom_first_pass bloom_second_pass;
	(*3: denoising*)
	denoise w h denoised_buffer post_process_buffer normal_buffer pos_buffer base_color_buffer;
	(*4: last loop, post processing*)
	set_color 0x5F9EA0;
	for x=0 to w-1 do
		for y=0 to h-1 do
			let fragCoord = vec3((float_of_int x), (float_of_int (h - 1 - y)), 0.0) in
			let sampler = if denoising_depth mod 2 = 0 || not !denoising then denoised_buffer else post_process_buffer in
			let col = post_process_frag x y w h sampler.(y).(x) fragCoord resolution bloom_second_pass in
			if adaptive_sampling then post_process_buffer.(y).(x) <- col;
			image_array.(y).(x) <- to_col255 col; 
		done; 
		if x mod 4 = 0 then aff_progress x h; (*loading bar*)
	done;
	
	Printf.printf "frame n°%d, took %f s" (accu + 1) ((Sys.time ()) -. t0);
	print_newline();
	let img = make_image image_array in (*create an image from a color array array and then draw this image*)
	draw_image img 0 0;;

let main () = 
	let w = size_x() in
	let h = size_y() in
	let bloom_first_pass = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in (*create all big arrays once before the loop*)
	let bloom_second_pass = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	(*denoising buffers*)
	let normal_buffer =  Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	let base_color_buffer =  Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	let pos_buffer = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	let denoised_buffer = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	(*adaptive sampling buffer*)
	let post_prosses_buffer = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	let is_active_buffer = Array.make_matrix h w true in
	(*mains buffers with color of the scene*)
	let main_buffer = Array.make_matrix h w (vec3(0.0, 0.0, 0.0)) in
	let image = Array.make_matrix h w 0x000000 in
	(*init ref*)
	let accu = ref 0 in (*number of frames accumulated*)
	_DOF := -1.0; (*-1.0 means no depth of field*)
	denoising := false;
	clear_graph();
	(*main loop*)
	while true do
		try
			newframe w h !accu main_buffer image bloom_first_pass bloom_second_pass normal_buffer base_color_buffer pos_buffer denoised_buffer post_prosses_buffer is_active_buffer;
			accu := !accu + 1;
		with
			|Restart -> begin clear_graph(); accu:=0 end
			|e -> raise e
	done;;

init 300 300;;
main();;
(**)




























