open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  let gray pixel : Pixel.t =
    let red = Pixel.red pixel in
    let green = Pixel.green pixel in
    let blue = Pixel.blue pixel in
    let grey = (red + green + blue) / 3 in
    grey, grey, grey
  in
  Image.map image ~f:gray
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
