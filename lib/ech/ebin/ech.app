%%% -*-erlang-*-
{application, ech,
 [{description, "Embedded CouchDB"},
  {vsn, "0.0"},
  {modules, []},
  {registered, []},
  {applications, [kernel,
                  stdlib,
                  sasl,
                  crypto]},
  {mod, {ech_app, []}}]}.
