# ocaml_rtx

projet de path tracing en ocaml.

to make it work, write the path of the .vox file in the variable 'path' in the beginging of the stript,
then, recompile the all thing.

# commandes:
  - mouse click on the image to set the focus on the point of the object clicked.
  - push space bar to toggle denoising

 - changing the size of the window can be done before but not during rendering
 - the function 'main' start the rendering process
 - to use adaptive sampling, set adaptive_sampling to true in the beginnig of the program

# denoising:
push space bar to toggle denoising 
(update in the beginning of the next frame if toggle durring post processing period) 

example, raw vs denoised:

-monu7 (one sample per pixel, denoising depth 4) 
<p align="center">
  <img alt="Light" src="./renders/monu7_1SPP.png" width="45%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="./renders/monu7_1SPP_denoised.png" width="45%">
</p>
-GI (8 sample per pixel, denoising depth 4, sigma_rt 40.0)
<p align="center">
  <img alt="Light" src="./renders/GL_noise.png" width="45%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="./renders/GI_denoised.png" width="45%">
</p>

# adaptive sampling:

to use adaptive sampling, set the const 'adaptive_sampling' to true in the beginnig of the program

the idea of adaptive sampling is that somme places are more noisy and need more rays so the renderer only shoot rays where needed,

this is an example of performance with adaptive sampling on 'metal2.vox':
<p align="center">
  <img src="./renders/adapt_plot.png" width="80%">
</p>

# renders:

<p align="center">
  <img src="./renders/chr_knight_dof.png">
  <img src="./renders/monu7.png">
  <img src="./renders/monu16.png">
  <img src="./renders/monu4.png">
  <img src="./renders/mushroom2.png">
  <img src="./renders/metal2.png">
  <img src="./renders/castle.png">
  <img src="./renders/GI.png">
  <img src="./renders/chess.png">
</p>

