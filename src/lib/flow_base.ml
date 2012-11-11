open Core.Std
  
include  Monad.Make2(struct 
  type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

  let return x = Lwt.return (Ok x) 
  let bind x f = 
    Lwt.bind x (function
    | Error e -> Lwt.return (Error e)
    | Ok o -> f o)
end)
type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

let error e = Lwt.return (Error e)

let (>><) m f = Lwt.bind m f

let bind_on_error m ~f = Lwt.bind m (function
  | Ok o -> Lwt.return (Ok o)
  | Error e -> (f e))
  
let double_bind m ~ok ~error = 
  Lwt.bind m (function
  | Ok o -> ok o
  | Error e -> error e)
    
let catch_io ~f x =
  Lwt.catch 
    (fun () -> 
      let a_exn_m : 'a Lwt.t = f x in
      Lwt.bind a_exn_m (fun x -> Lwt.return (Ok x)))
    (fun e -> Lwt.return (Error e))
    
let wrap_io ?(on_exn=fun e -> `io_exn e) ~f x =        
  let caught = catch_io f x in
  double_bind caught
    ~ok:return
    ~error:(fun exn -> error (on_exn exn)) 

let wrap ~on_exn ~f x =
  let caught = catch_io (fun a -> Lwt.return (f a)) x in
  double_bind caught
    ~ok:return
    ~error:(fun exn -> error (on_exn exn))
  
let map_option: 'a option -> f:('a -> ('b, 'error) t) -> ('b option, 'error) t
  = fun o ~f ->
    begin match o with
    | None -> return None
    | Some s ->
      f s
      >>= fun g ->
      return (Some g)
    end
    
let of_result r = Lwt.return r 

