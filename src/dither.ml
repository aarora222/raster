open Core

(* This should look familiar by now! *)
let transform image =
  let dither_img ~x ~y image pixel =
    let value = Pixel.red pixel // Image.max_val image in
    let newval =
      match Float.O.(0.5 < value) with
      | true -> Image.max_val image
      | false -> 0
    in
    let error = Pixel.red pixel - newval in
    if x - 1 > 0 && y + 1 < Image.height image
    then (
      let downleft = Image.get image ~x:(x - 1) ~y:(y + 1) |> Pixel.red in
      Image.set
        image
        ~x:(x - 1)
        ~y:(y + 1)
        ((error * 3 / 16) + downleft |> Pixel.of_int));
    if x + 1 < Image.width image
    then (
      let right = Image.get image ~x:(x + 1) ~y |> Pixel.red in
      Image.set image ~x:(x + 1) ~y ((error * 7 / 16) + right |> Pixel.of_int));
    if x + 1 < Image.width image && y + 1 < Image.height image
    then (
      let downright = Image.get image ~x:(x + 1) ~y:(y + 1) |> Pixel.red in
      Image.set
        image
        ~x:(x + 1)
        ~y:(y + 1)
        ((error / 16) + downright |> Pixel.of_int));
    if y + 1 < Image.height image
    then (
      let down = Image.get image ~x ~y:(y + 1) |> Pixel.red in
      Image.set image ~x ~y:(y + 1) ((error * 5 / 16) + down |> Pixel.of_int));
    Image.set image ~x ~y (Pixel.of_int newval);
    image
  in
  Image.foldi ~init:image image ~f:dither_img
;;

(*Image.foldi image ~init:image ~f:transform_pix*)

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
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
