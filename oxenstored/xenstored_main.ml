let _ : (unit -> unit) * Store.t * Connections.t * Domains.domains =
  Xenstored.main ()
