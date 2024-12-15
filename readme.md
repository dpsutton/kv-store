Setting up

```
~/projects/ocaml
❯ dune init proj --kind=lib file_kv
Entering directory '/Users/dan/projects/ocaml/file_kv'
Success: initialized project component named file_kv
```

```
projects/ocaml/file_kv via 🐫 v4.14.0 (4.14.0)
❯ opam switch create file_kv 5.2.1

<><> Installing new switch packages <><><><><><><><><><><><><><><><><><><><>  🐫
Switch invariant: ["ocaml-base-compiler" {= "5.2.1"} | "ocaml-system" {= "5.2.1"}]

<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><>  🐫
⬇ retrieved ocaml-config.3  (cached)
⬇ retrieved ocaml-system.5.2.1  (cached)
∗ installed base-bigarray.base
∗ installed base-threads.base
∗ installed base-unix.base
∗ installed host-arch-arm64.1
∗ installed host-system-other.1
∗ installed ocaml-system.5.2.1
∗ installed ocaml-config.3
∗ installed ocaml.5.2.1
∗ installed base-domains.base
∗ installed base-nnp.base
Done.

❯ eval $(opam env)

❯ opam install dune
The following actions will be performed:
=== install 1 package
  ∗ dune 3.17.0

<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><>  🐫
⬇ retrieved dune.3.17.0  (cached)
∗ installed dune.3.17.0
Done.

projects/ocaml/file_kv via 🐫 v5.2.1 (file_kv)
❯ opam install merlin tuareg

projects/ocaml/file_kv via 🐫 v5.2.1 (file_kv)
❯ dune build

projects/ocaml/file_kv via 🐫 v5.2.1 (file_kv)
❯
```


todo:
- [ ] shorter commands to get? alias? `alias g="kv-client get"` and `echo "$(g db-arg)=$(g pg-prefix)clean $(g token-arg)=$(g ee)"`
- [x] add a way to keep looking up token={{token-arg}}={{ee}} expands the token-arg and ee
- [x] handle spaces, etc in seed data. `quick=this is stuff` should work.
- [ ] ability to reload init file. editing text.data with other k/v's and then get it to reload without dumping the whole other k/v's set in memory
- [ ] interpolate a string: `kv interpolate "PORT=~p config_file=~config ..." type interpolation


building:

just `dune build` and then `dune install` puts it at `_build/default/bin`. puts .exe on it.
