
val test_case
  : ?timeout:Core_kernel.Time.Span.t
  -> string
  -> Alcotest.speed_level
  -> ('a -> unit Async_kernel.Deferred.t)
  -> 'a Alcotest.test_case
