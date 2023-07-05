open Core

let valid_coord x y image = 
  let height = Image.height image in
  let width = Image.width image in
  if(x < width && y < height) then true else false
;; 

(* This should look familiar by now! *)
let transform image = 
  let gray_img = Grayscale.transform image in 
  let transform_pix ~x ~y pixel = 
    let value = Pixel.red pixel in
    let error = (match (value > 128) with
    | true -> 255 - value
    | false -> 0 - value) in 
    let error = Float.of_int error in 
    let right_weight = Float.( * ) (Float.(/) 7.0 16.0) error in 
    let sw_weight = Float.( * ) (Float.(/) 3.0 16.0) error in 
    let down_weight = Float.( * ) (Float.(/) 5.0 16.0) error in
    let se_weight = Float.( * ) (Float.(/) 1.0 16.0) error in 
    if(valid_coord (x+1) y image) then (Image.set ~x: (x + 1) ~y (Image))
  in 




let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
