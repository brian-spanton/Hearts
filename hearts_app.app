{application, hearts_app,
 [{description, "Hearts"},
  {vsn, "1"},
  {modules, [hearts_app, hearts_server, hearts, cards]},
  {registered, [lobby]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {hearts_app,[]}}
 ]}.
