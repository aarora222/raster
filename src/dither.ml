open Core

let valid_coord x y image =
  let height = Image.height image in
  let width = Image.width image in
  if x < width && y < height && y >= 0 && x >= 0 then true else false
;;

let get_contrasted_image image =
  let final_transform ~x ~y image pixel : Image.t =
    let value = Pixel.red pixel in
    let _b =
      match
        Float.( < )
          0.5
          (Float.( / )
             (Float.of_int value)
             (Float.of_int (Image.max_val image)))
      with
      | true -> Image.set image ~x ~y (Pixel.of_int (Image.max_val image))
      | false -> Image.set image ~x ~y (Pixel.of_int 0)
    in
    image
  in
  Image.foldi image ~init:image ~f:final_transform
;;

(* This should look familiar by now! *)
let transform image =
  (*let transformed_image = get_contrasted_image image in *)
  let image = Grayscale.transform image in
  let transformed_img = get_contrasted_image image in
  let transform_pix ~x ~y img pixel =
    let value = Pixel.red pixel in
    let error =
      match value > Image.max_val img / 2 with
      | true ->
        Image.set img ~x ~y (Pixel.of_int (Image.max_val img));
        Image.max_val img - value
      | false ->
        Image.set img ~x ~y (Pixel.of_int 0);
        0 - value
    in
    let error = Float.of_int error in
    let right_weight = Float.( * ) (Float.( / ) 7.0 16.0) error in
    let sw_weight = Float.( * ) (Float.( / ) 3.0 16.0) error in
    let down_weight = Float.( * ) (Float.( / ) 5.0 16.0) error in
    let se_weight = Float.( * ) (Float.( / ) 1.0 16.0) error in
    if valid_coord (x + 1) y img
    then
      Image.set
        img
        ~x:(x + 1)
        ~y
        (Pixel.( + )
           (Image.get img ~x:(x + 1) ~y)
           (Pixel.of_int (Int.of_float right_weight)));
    if valid_coord x (y - 1) img
    then
      Image.set
        img
        ~x
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get img ~x ~y:(y - 1))
           (Pixel.of_int (Int.of_float down_weight)));
    if valid_coord (x - 1) (y - 1) img
    then
      Image.set
        img
        ~x:(x - 1)
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get img ~x:(x - 1) ~y:(y - 1))
           (Pixel.of_int (Int.of_float sw_weight)));
    if valid_coord (x + 1) (y - 1) img
    then
      Image.set
        img
        ~x:(x + 1)
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get img ~x:(x + 1) ~y:(y - 1))
           (Pixel.of_int (Int.of_float se_weight)));
    img
  in
  Image.foldi transformed_img ~init:transformed_img ~f:transform_pix
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
