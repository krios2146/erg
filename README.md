# Erg

> Erg - vast area of shifting sand dunes within a desert, also called a sand sea. Ergs are found in places like the Sahara Desert

Library that implements part of the [HTTP/1.1 standard](https://datatracker.ietf.org/doc/html/rfc2616) from scratch with practically zero dependencies, offering a lightweight API for clients

![wakatime](https://wakatime.com/badge/user/465e62bb-2db9-4043-9075-8cefadda6d94/project/71533279-e67b-4534-a090-2d54dd788e61.svg)

> [!WARNING]  
> This project was built to learn OCaml and HTTP, it literally has no tests
> 
> **Do not use it in any real software**

## Features

- Persistent TCP connections
- Multithreading
- Simple routing

## Installation

Library is not distributed via [opam](https://opam.ocaml.org/) deliberately

1. This is a learning project, it barely provides any value to anyone, but me
2. I wouldn't maintain it
3. Erg is a cool name and I don't want to take it

---

Instead of installing it with opam you'll need to do it manually

Assuming you created have some [dune](https://dune.build/) project, all you need to do is clone the erg to `lib/erg` like so

```bash
git clone git@github.com:krios2146/erg.git lib/erg
```

Then just add it to the dune file under the libraries stanza like so

```
(executable
 (public_name your_project_name)
 (name main)
 (libraries erg))
```

You can access `Erg` module in your code now

## Example

Simple example that shows most of the Erg features:

```ocaml
(* Define function that transform HTTP request to HTTP resposne *)
let process_hello req = 
  (* Read query parameter `name` from the URL *)
  let name =
    match Erg.Http_request.get_param req "name" with
    | None -> "World"
    | Some n -> n
  in
  Erg.Http_response.empty
  |> Erg.Http_response.set_status_code Erg.Status_code.OK
  |> Erg.Http_response.set_header (Erg.Http_header.ContentType "text/plain")
  (* Create response body based on the `name` parameter *)
  |> Erg.Http_response.set_response_body ("Hello, " ^ name ^ "!")
;;

(* Create handler for GET request on the `/hello` URL *)
let hello_handler = Erg.create_handler Erg.Http_method.Get "/hello" process_hello in
(* Add `hello_handler` to the Erg handlers *)
let handlers = 
  Erg.empty_handlers ()
  |> Erg.add_handler handler
in
(* Start Erg on the port 8080 *)
Erg.start 8080 handlers
```

## API Reference

Contents of the `Erg.mli`

```ocaml
module Http_method : sig
  type t =
    | Get
    | Post
    | Put
    | Delete
end

module Http_header : sig
  type t =
    (* General Headers *)
    | CacheControl of string
    | Connection of string
    | Date of string
    | Pragma of string
    | Trailer of string
    | TransferEncoding of string
    | Upgrade of string
    | Via of string
    | Warning of string
    (* Request Headers *)
    | Accept of string
    | AcceptCharset of string
    | AcceptEncoding of string
    | AcceptLanguage of string
    | Authorization of string
    | Expect of string
    | From of string
    | Host of string
    | IfMatch of string
    | IfModifiedSince of string
    | IfNoneMatch of string
    | IfRange of string
    | IfUnmodifiedSince of string
    | MaxForwards of string
    | ProxyAuthorization of string
    | Range of string
    | Referer of string
    | TE of string
    | UserAgent of string
    (* Entity Headers *)
    | Allow of string
    | ContentEncoding of string
    | ContentLanguage of string
    | ContentLength of string
    | ContentLocation of string
    | ContentMD5 of string
    | ContentRange of string
    | ContentType of string
    | Expires of string
    | LastModified of string
end

module Status_code : sig
  type t =
    | Continue (** 100 *)
    | Switching_Protocols (** 101 *)
    | OK (** 200 *)
    | Created (** 201 *)
    | Accepted (** 202 *)
    | Non_Authoritative_Information (** 203 *)
    | No_Content (** 204 *)
    | Reset_Content (** 205 *)
    | Partial_Content (** 206 *)
    | Multiple_Choices (** 300 *)
    | Moved_Permanently (** 301 *)
    | Found (** 302 *)
    | See_Other (** 303 *)
    | Not_Modified (** 304 *)
    | Use_Proxy (** 305 *)
    | Temporary_Redirect (** 307 *)
    | Bad_Request (** 400 *)
    | Unauthorized (** 401 *)
    | Payment_Required (** 402 *)
    | Forbidden (** 403 *)
    | Not_Found (** 404 *)
    | Method_Not_Allowed (** 405 *)
    | Not_Acceptable (** 406 *)
    | Proxy_Authentication_Required (** 407 *)
    | Request_Timeout (** 408 *)
    | Conflict (** 409 *)
    | Gone (** 410 *)
    | Length_Required (** 411 *)
    | Precondition_Failed (** 412 *)
    | Request_Entity_Too_Large (** 413 *)
    | Request_URI_Too_Long (** 414 *)
    | Unsupported_Media_Type (** 415 *)
    | Requested_Range_Not_Satisfiable (** 416 *)
    | Expectation_Failed (** 417 *)
    | Internal_Server_Error (** 500 *)
    | Not_Implemented (** 501 *)
    | Bad_Gateway (** 502 *)
    | Service_Unavailable (** 503 *)
    | Gateway_Timeout (** 504 *)
    | HTTP_Version_Not_Supported (** 505 *)
end

module Http_response : sig
  type t

  (** [empty] Returns "empty" HTTP response that must be filled with at least status code *)
  val empty : t

  (** [set_headers res h_list] Sets specified headers to the response, returns response with headers *)
  val set_headers : Http_header.t list -> t -> t

  (** [set_header res h] Sets specified header to the response, returns response with header *)
  val set_header : Http_header.t -> t -> t

  (** [set_response_body res body] Sets response body to the response, returns response with a body *)
  val set_response_body : string -> t -> t

  (** [set_status_code res code] Sets status code for the response, returns response with specified status code *)
  val set_status_code : Status_code.t -> t -> t
end

module Http_request : sig
  type t

  (** [get_param req param] Retrieves the optional GET parameter from the request's query *)
  val get_param : t -> string -> string option

  (** [get_headers req] Retrieves all headers from the request *)
  val get_headers : t -> Http_header.t list

  (** [get_body req] Retrieves untrimmed body of the request as a string *)
  val get_body : t -> string
end

type handler
type handlers

(** [empty_handlers] Returns empty handlers that should be filled with handlers with [add_handler] *)
val empty_handlers : unit -> handlers

(** [add_handler h handlers] Adds the specified handler to the handlers *)
val add_handler : handler -> handlers -> handlers

(** [create_handler m uri req -> res]

    Creates the [handler] for the specified HTTP method on the [uri]
    with function that transforms [http_request] to [http_response] *)
val create_handler
  :  Http_method.t
  -> string
  -> (Http_request.t -> Http_response.t)
  -> handler

(** [start port handlers] Starts the server listening on the specified port with specified handlers *)
val start : int -> handlers -> unit
```
