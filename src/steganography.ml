open Core

let change_fruit_img img =
  let change_pix p : Pixel.t =
    let r, g, b = p in
    r % 4 * 64, g % 4 * 64, b % 4 * 64
  in
  Image.map img ~f:change_pix
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
        let image = Image.load_ppm ~filename |> change_fruit_img in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_stenography.ppm")]
;;
