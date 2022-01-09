# ocaml_rtx

projet de path tracing en ocaml.

to make it work, write the path of the .vox file in the variable 'path' in the beginging of the stript,
then, recompile the all thing.

# commandes:
  - mouse click on the image to set the focus on the point of the object clicked.
  - push space bar to toggle denoising

(to use adaptive sampling, set adaptive_sampling to true in the beginnig of the program)
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

# denoising:
push space bar to toggle denoising 
(update in the beginning of the next frame if toggle durring post processing periode) 


one sample per pixel, raw vs denoised example:
<p align="center">
  <img alt="Light" src="./renders/monu7_1SPP.png" width="45%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="./renders/monu7_1SPP_denoised.png" width="45%">
</p>
