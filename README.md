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

will compile and run the program; giving you output similar to:

```
[info] welcome to sbt 1.12.3 (Eclipse Adoptium Java 25.0.1)
[info] loading project definition from /home/sbtuser/project
[info] loading settings for project root from build.sbt...
[info] set current project to PokerCalulator (in build file:/home/sbtuser/)
[info] running pcalc.Main 
10
Flop: [3♣|8♠|J♠] -> (HighCard,List(3♣, 8♠, J♠))
HoleCards: Vector([7♥|8♣], [T♠|A♥])
PlayerHands: Vector((Pair,List(3♣, 7♥, J♠, 8♠, 8♣)), (HighCard,List(3♣, 8♠, T♠, J♠, A♥)))
Turn: [3♣|8♠|J♠|K♥], Hs: Vector(Pair, HighCard)
River: [3♣|8♠|J♠|K♥|9♥], Hs: Vector((Pair,List(9♥, J♠, K♥, 8♠, 8♣)), (HighCard,List(9♥, T♠, J♠, K♥, A♥)))

[...similar content redacted...]

Flop: [5♥|5♣|Q♦] -> (Pair,List(Q♦, 5♥, 5♣))
HoleCards: Vector([2♠|T♠], [2♥|6♠])
PlayerHands: Vector((Pair,List(2♠, T♠, Q♦, 5♥, 5♣)), (Pair,List(2♥, 6♠, Q♦, 5♥, 5♣)))
Turn: [5♥|5♣|Q♦|8♦], Hs: Vector(Pair, Pair)
River: [5♥|5♣|Q♦|8♦|7♣], Hs: Vector((Pair,List(8♦, T♠, Q♦, 5♥, 5♣)), (Pair,List(7♣, 8♦, Q♦, 5♥, 5♣)))

[success] Total time: 1 s, completed Feb 20, 2026, 5:47:15 PM
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
chmod -R g=u workdir/
```
