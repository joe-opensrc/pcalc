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

#### Running the container 

```bash
docker run -v "$(pwd)"/workdir:/home/sbtuser -it joe-opensrc/sbt:1.12.3 sbt ~run 
```

#### Possible env requirements

If you're using docker in a non-suid mode, \
you will probably need to set the appropriate \ 
permissions / ownership on the workdir.

You can try something like this:

Assuming /etc/subgid has an entry similar to: \
yourgroup:100000:65536

```bash 
# set group ownership
# note, sbtuser is 1001 inside the container
chown -R :101001 workdir/

# set group permissions to the user permissions
chmod -R +g=u workdir/   
```
