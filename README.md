# BOTWR

### Local development
```bash
dune clean && dune build src/app.bc.js files/style.css -w

python3 -m http.server --directory files/ 8081
```

### Deploy
```bash
dune clean && DUNE_PROFILE=release dune build src/app.bc.js files/style.css

./scripts/deploy.sh
```

#### Test deployment image
```bash
docker build . -t botwr:latest

docker run -it --rm -p 8081:8081 botwr:latest
```
