let () =
  Client.send_mail
    ~server:("smtp.laposte.net", 465)
    ~auth:("Zm9vLmJhci5mb29AbGFwb3N0ZS5uZXQ=", "Rm9vMC5CYXIx")
    ~from:(Some "me", "foo.bar.foo@laposte.net")
    ~to_:(Some "someone", "someone@gmail.com")
    ~content:"text/plain"
    "Blabla"
    "Hello world"
  |> Lwt_main.run
