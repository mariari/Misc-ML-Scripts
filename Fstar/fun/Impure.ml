open Prims
let (my_arr : Prims.int FStar_Array.array) =
  let my_arr =
    FStar_Array.of_seq
      (FStar_Seq_Properties.of_list
         [(Prims.parse_int "1");
         (Prims.parse_int "2");
         (Prims.parse_int "3");
         (Prims.parse_int "4");
         (Prims.parse_int "5")]) in
  (let uu____3536 = FStar_Array.index my_arr (Prims.parse_int "0") in ());
  my_arr
let (operation : unit -> Prims.int) =
  fun uu____3551 -> FStar_Array.index my_arr (Prims.parse_int "0")
let (foo : Prims.int) =
  let my_arr1 =
    FStar_Array.create (Prims.parse_int "10") (Prims.parse_int "5") in
  FStar_Array.index my_arr1 (Prims.parse_int "0")
let main : 'Auu____3579 . 'Auu____3579 -> Prims.int Prims.list =
  fun x ->
    let original_index = operation () in
    let index1 = FStar_Array.index my_arr (Prims.parse_int "0") in
    FStar_Array.upd my_arr (Prims.parse_int "0")
      (index1 + (Prims.parse_int "10"));
    (let index2 = FStar_Array.index my_arr (Prims.parse_int "0") in
     FStar_Array.upd my_arr (Prims.parse_int "0")
       (index2 + (Prims.parse_int "10"));
     (let uu____3621 = operation () in [uu____3621; original_index]))