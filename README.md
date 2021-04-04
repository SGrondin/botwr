# BOTWR

```bash
dune build src/app.bc.js files/ -w

python3 -m http.server --directory files/ 8081
```

```bash
dune clean && DUNE_PROFILE=release dune build src/app.bc.js files/

docker build . -t botwr:latest

docker run -it --rm -p 8081:8081 botwr:latest

./scripts/deploy.sh
```
