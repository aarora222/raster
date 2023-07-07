open Core

let transform image threshold =
  let invert_pix x y pixel = 
    let maxval = Image.max_val image in 
    let current_val = Pixel.red pixel in 
    
  in 

;;