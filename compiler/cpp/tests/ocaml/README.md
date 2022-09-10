## Testing approach

1. Programmatically construct parsed instances of Thrift IDLs using internal
   types
2. Generate the OCaml output using the OCaml generator
3. Capture the generated output in `ostringstream`
4. Query and compare the outputs in the strings to stored snapshots in the
   `snapshot_*.cc` files

Run tests in `../tests` directory:

      # Only on changing build definition:
      cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/bison -DCMAKE_CXX_STANDARD=11 .

      # On each iteration:
      rm -rf gen-ocaml; cmake --build . && ctest --output-on-failure
