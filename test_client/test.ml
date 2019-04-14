let () =
  Client.send_mail
    ~server:("smtp.laposte.net", 465)
    ~from:(Some "me", "foo.bar.foo@laposte.net")
    ~to_:(Some "someone", "someone@gmail.com")
    ~content:"text/plain"
    "Blabla"
    "Hello world"
  |> Lwt_main.run
