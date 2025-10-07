# Inertia.js Adapter for Dream

This project provides an [Inertia.js](https://inertiajs.com/) adapter for the [Dream](https://aantron.github.io/dream/) web framework for OCaml. It allows you to build modern single-page applications with server-side routing and controllers, while using a JavaScript-powered frontend.

## Getting Started

To use the library in your project, add it to your `dune` file:

```dune
(libraries dream_inertia)
```

Then, initialize the Inertia driver and add the middleware to your Dream application:

```ocaml
let () =
  let root_view = Inertia.Helper.root_view ~js:"/assets/main.js" ~css:"/assets/main.css" in

  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.flash
  @@ Inertia.middleware ~version:"1.0.0" ~root_view ()
  @@ Dream.router [ (* your routes *) ]
```

## Example

This project includes a full-stack example application that demonstrates how to use the library. The backend is built with Dream and the frontend is built with Vue.js and Vite.

### Running the Example

1.  **Install dependencies:**

    ```bash
    opam install . --deps-only
    cd example
    npm install
    ```

2.  **Build and run the application:**

    ```bash
    cd example
    npm run build
    dune exec ./src/bin/main.exe
    ```

The application will be available at `http://localhost:8080`.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE.md](LICENSE.md) file for details.
