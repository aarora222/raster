open Core

let valid_coord x y image =
  let height = Image.height image in
  let width = Image.width image in
  if x < width && y < height && y >= 0 && x >= 0 then true else false
;;

let get_board_positions image =
  let height = Image.height image in
  let width = Image.width image in
  let positions =
    List.init width ~f:(fun x -> List.init height ~f:(fun y -> x, y))
  in
  List.concat positions
;;

(* This should look familiar by now! *)
let transform image =
  let image = Grayscale.transform image in
  let transform_pix ~x ~y pixel =
    let value = Pixel.red pixel in
    let error =
      match value > 128 with
      | true ->
        Image.set image ~x ~y (255, 255, 255);
        255 - value
      | false ->
        Image.set image ~x ~y (0, 0, 0);
        0 - value
    in
    let error = Float.of_int error in
    let right_weight = Float.( * ) (Float.( / ) 7.0 16.0) error in
    let sw_weight = Float.( * ) (Float.( / ) 3.0 16.0) error in
    let down_weight = Float.( * ) (Float.( / ) 5.0 16.0) error in
    let se_weight = Float.( * ) (Float.( / ) 1.0 16.0) error in
    if valid_coord (x + 1) y image
    then
      Image.set
        image
        ~x:(x + 1)
        ~y
        (Pixel.( + )
           (Image.get image ~x:(x + 1) ~y)
           ( Int.of_float right_weight
           , Int.of_float right_weight
           , Int.of_float right_weight ));
    if valid_coord x (y - 1) image
    then
      Image.set
        image
        ~x
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get image ~x ~y:(y - 1))
           ( Int.of_float down_weight
           , Int.of_float down_weight
           , Int.of_float down_weight ));
    if valid_coord (x - 1) (y - 1) image
    then
      Image.set
        image
        ~x:(x - 1)
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get image ~x:(x - 1) ~y:(y - 1))
           ( Int.of_float sw_weight
           , Int.of_float sw_weight
           , Int.of_float sw_weight ));
    if valid_coord (x + 1) (y - 1) image
    then
      Image.set
        image
        ~x:(x + 1)
        ~y:(y - 1)
        (Pixel.( + )
           (Image.get image ~x:(x + 1) ~y:(y - 1))
           ( Int.of_float se_weight
           , Int.of_float se_weight
           , Int.of_float se_weight ));
    let pix_val = Image.get image ~x ~y in
    print_s [%message "Pix value: " (pix_val : Pixel.t)]
  in
  let img_positions = get_board_positions image in
  let rec dither_img ~(img_positions : (int * int) list) =
    match img_positions with
    | [] -> ()
    | (x, y) :: tail ->
      transform_pix ~x ~y (Image.get image ~x ~y);
      dither_img ~img_positions:tail
  in
  dither_img ~img_positions;
  image
;;

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
