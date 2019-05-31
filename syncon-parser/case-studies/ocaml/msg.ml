
open Ustring.Op

type row = int
type col = int
type filename = ustring
type info =
  | Info of filename * row * col * row * col
  | NoInfo



type id =
  | LEX_UNKNOWN_CHAR
  | LEX_COMMENT_NOT_TERMINATED
  | LEX_STRING_NOT_TERMINATED
  | LEX_INVALID_ESCAPE
  | PARSE_ERROR
  | ERROR of string


type severity =
  | ERROR
  | WARNING

type arguments = ustring list

type message = id * severity * info * arguments


exception Error of message




let id2str id =
  match id  with
    | LEX_UNKNOWN_CHAR -> us"Unknown character"
    | LEX_COMMENT_NOT_TERMINATED -> us"Comment is not terminated"
    | LEX_STRING_NOT_TERMINATED -> us"String is not terminated"
    | LEX_INVALID_ESCAPE -> us"Invalid escape characters"
    | PARSE_ERROR -> us"Parse error"
    | ERROR msg -> us msg

let severity2str s =
  match s with
    | ERROR -> us"ERROR"
    | WARNING -> us"WARNING"

let info2str_startline fi =
  match fi with
    | Info(_,l1,_,_,_) -> l1
    | NoInfo -> assert false

let message2str (id,sev,info,_)  =
  match info with
    | Info(filename,l1,c1,l2,c2) ->
	begin
	  us"FILE \"" ^. filename ^. us"\" " ^.
	    (ustring_of_int l1) ^. us":" ^.
	    (ustring_of_int c1) ^. us"-" ^.
	    (ustring_of_int l2) ^. us":" ^.
	    (ustring_of_int c2) ^. us" " ^.
	    (severity2str sev) ^. us": " ^.
	    (id2str id)
        end
    |  NoInfo -> us"NO INFO: " ^. (id2str id)

let raise_error fi msg =
  raise (Error (ERROR(msg),ERROR,fi,[]))

let error fi msg = raise_error fi (msg |> Ustring.to_utf8)
