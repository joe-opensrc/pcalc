Poker Calculator!
-----------------

Basically a simple poker dealer + hand ranking algorithm.

The main aim being to learn me some scala :)

---

#### Install / Dev-Setup:

```bash
git clone https://github.com/joe-opensrc/pcalc
cd pcalc
docker build -t joe-opensrc/sbt:v1.12.3 .  # or whatever name you like ;)
```

If you're using docker in a non-suid mode, \
you will have to set the workdir/ to the appropriate user \
(which ironically requires sudo on the host)

```bash 
chown -R 101001 workdir/
```

#### Running Container 

```bash
docker run -v "$(pwd)"/workdir:/home/sbtuser -it joe-opensrc/sbt:1.12.3 sbt ~run 
```
