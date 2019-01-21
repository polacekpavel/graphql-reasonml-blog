type user = {
  id: int,
  name: string,
};

let alice: user = {id: 1, name: "Alice" }
let bob: user = {id: 2, name: "Bob" };
let users: list(user) = [alice, bob];

let user =
  Graphql_lwt.Schema.(
    obj("user", ~fields= _ =>
      [
        field("id", ~args=[], ~typ=non_null(int), ~resolve=(_, p) =>
          p.id
        ),
        field("name", ~args=[], ~typ=non_null(string), ~resolve=(_, p) =>
          p.name
        )
      ]
    )
  );

let schema =
  Graphql_lwt.Schema.(
    schema(
      [
        io_field(
          "users",
          ~args=[],
          ~typ=non_null(list(non_null(user))),
          ~resolve=((), ()) =>
          Lwt_result.return(users)
        ),
      ],
    )
  );

let () = Graphql_lwt.Server.start(~ctx=_req => (), schema) |> Lwt_main.run;
