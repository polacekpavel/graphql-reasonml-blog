open Lwt;

module C = Cohttp_lwt_unix;

open Graphql_lwt;

type role =
  | User
  | Admin;

type user = {
  id: int,
  name: string,
  role,
  friends: list(user)
};

let rec alice = {id: 1, name: "Alice", role: Admin, friends: [bob]}
and bob = {id: 2, name: "Bob", role: User, friends: [alice]};

let users = [alice, bob];

let role =
  Schema.(
    enum(
      "role",
      ~values=[
        enum_value("USER", ~value=User, ~doc="A regular user"),
        enum_value("ADMIN", ~value=Admin, ~doc="An admin user")
      ]
    )
  );

let user =
  Schema.(
    obj(
      "user",
      ~fields=
        (user) => [
          field("id", ~args=[], ~typ=non_null(int), ~resolve=((), p) => p.id),
          field("name", ~args=[], ~typ=non_null(string), ~resolve=((), p) => p.name),
          field("role", ~args=[], ~typ=non_null(role), ~resolve=((), p) => p.role),
          field(
            "friends",
            ~args=[],
            ~typ=list(non_null(user)),
            ~resolve=((), p) => Some(p.friends)
          )
        ]
    )
  );

let rec consume_stream = (stream) =>
  Lwt.catch(
    () =>
      Lwt_stream.next(stream)
      >>= (
        (x) => {
          let Ok(x) | Error(x) = x;
          Printf.eprintf("stream response: '%s'\n%!", Yojson.Basic.to_string(x));
          if (Lwt_stream.is_closed(stream)) {
            Lwt.return_unit;
          } else {
            consume_stream(stream);
          };
        }
      ),
    fun
    | Lwt_stream.Closed
    | Lwt_stream.Empty => Lwt.return_unit
    | _ => Lwt.return_unit
  );

let set_interval = (s, f, destroy) => {
  let rec set_interval_loop = (s, f, n) => {
    let timeout =
      Lwt_timeout.create(
        s,
        () =>
          if (n > 0) {
            f();
            set_interval_loop(s, f, n - 1);
          } else {
            destroy();
          }
      );
    Lwt_timeout.start(timeout);
  };
  set_interval_loop(s, f, 5);
};

let schema =
  Schema.(
    schema(
      [
        io_field(
          "users",
          ~args=[],
          ~typ=non_null(list(non_null(user))),
          ~resolve=((), ()) => Lwt_result.return(users)
        ),
        field(
          "greeter",
          ~typ=string,
          ~args=
            Arg.[
              arg(
                "config",
                ~typ=
                  non_null(
                    obj(
                      "greeter_config",
                      ~coerce=(greeting, name) => (greeting, name),
                      ~fields=[
                        arg'("greeting", ~typ=string, ~default="hello"),
                        arg("name", ~typ=non_null(string))
                      ]
                    )
                  )
              )
            ],
          ~resolve=((), (), (greeting, name)) => Some(Format.sprintf("%s, %s", greeting, name))
        )
      ]
  )
);

let () = Server.start(~ctx=(_req) => (), schema) |> Lwt_main.run
