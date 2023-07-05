open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let height = Image.height image in
  let width = Image.width image in
  let avg_pix ~x ~y pixel =
    let _red = Pixel.red pixel in
    let x_up =
      match x + radius > width with true -> width - 1 | false -> x + radius
    in
    let y_up =
      match y + radius > height with
      | true -> height - 1
      | false -> y + radius
    in
    let x_down =
      match x - radius < 0 with true -> 0 | false -> x - radius
    in
    let y_down =
      match y - radius < 0 with true -> 0 | false -> y - radius
    in
    let pic_rad =
      Image.slice
        image
        ~x_start:x_down
        ~x_end:x_up
        ~y_start:y_down
        ~y_end:y_up
    in
    Image.mean_pixel pic_rad
  in
  Image.mapi image ~f:avg_pix
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
