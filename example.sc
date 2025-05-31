/* ===============================================================
   Escape-Room: „Das Arbeitszimmer“          Language-learning demo
   Target: ScummC (https://github.com/ScummVM/scummc)
   =============================================================== */

/* ---------------- Room and object IDs ---------------- */
#define ROOM_ID        1
#define ACTOR_PLAYER   1

#define OBJ_TUER           1
#define OBJ_TRESOR         2
#define OBJ_TRESOR_PANEL   3
#define OBJ_SCHREIBTISCH   4
#define OBJ_SCHUBLADE      5
#define OBJ_NOTIZ          6
#define OBJ_BUECHERREGAL   7
#define OBJ_ROTES_BUCH     8
#define OBJ_SCHLUESSEL     9
#define OBJ_POSTER         10
#define OBJ_WOERTERBUCH    11
#define OBJ_TUERGRIFF      12

/* ---------------- Room entry script ------------------ */
script ROOM_ID
{
    /* Put player into room */
    putActorInRoom(ACTOR_PLAYER, ROOM_ID);
    putActor( ACTOR_PLAYER, 160, 110 );      // Standing centre
    setCameraAt( ROOM_ID, 160 );

    walkActorTo(ACTOR_PLAYER, 160, 120);
    faceActor( ACTOR_PLAYER, 0 );            // Face south
    startScript( S_ROOM_IDLE );              // idle loop
}

/* ------------- Idle - waits for verb processing ------ */
script S_ROOM_IDLE
{
    while (1) {
        breakHere();     // keep script alive
    }
}

/* ======================================================
   === Object definitions (bitmap/box coords omitted) ===
   ====================================================== */

/*** Tür ***/
object OBJ_TUER "Tür"
{
    class  oClassDoor;
    verb   vOpenClose;               // uses default door behaviour
    verb   vLook
    {
        sayLine(ACTOR_PLAYER, "Die Tür ist verschlossen.");
    }
}

/*** Tresor ***/
object OBJ_TRESOR "Tresor"
{
    costume 2;        // obj002.png - closed safe
    state  0  open=0;
    class  oClassBox;
    verb   vOpen
    {
        if (getState(OBJ_TRESOR) == 1) {
            sayLine(ACTOR_PLAYER, "Er ist schon offen.");
        } else {
            sayLine(ACTOR_PLAYER, "Ich brauche einen Schlüssel.");
        }
    }
    verb   vUse
    {
        if (objectInHand(OBJ_SCHLUESSEL)) {
            setState(OBJ_TRESOR,1);          // unlock safe
            startScript( S_ASK_CODE );
        } else {
            sayLine(ACTOR_PLAYER,"Das funktioniert nicht.");
        }
    }
}

/*** Tresor-Panel (for code input) ***/
script S_ASK_CODE
{
    setActorTalking(ACTOR_PLAYER);
    string code = prompt("Gib den 3-stelligen Code ein:");  // German prompt
    if (code == "491") {
        sayLine(ACTOR_PLAYER, "Perfekt!");        // Correct
        setState(OBJ_TRESOR, 2);                  // completely open
        putActorInRoom(OBJ_TUERGRIFF, ROOM_ID);
        startScript( S_REVEAL_HANDLE );
    } else {
        sayLine(ACTOR_PLAYER, "Falsch…");
    }
}

script S_REVEAL_HANDLE
{
    animateObject(OBJ_TUERGRIFF, 1);   // small bounce animation
    sayLine(ACTOR_PLAYER, "Ein Türgriff! Genau was ich brauche.");
}

/*** Türgriff (appears only after safe opened) ***/
object OBJ_TUERGRIFF "Türgriff"
{
    costume 8;            // obj008.png - door handle
    initialRoom  0;            // hidden until spawned
    class  oClassPickup;
    verb   vUse
    {
        if (objectInHand(OBJ_TUERGRIFF) && verbObj == OBJ_TUER) {
            sayLine(ACTOR_PLAYER,"Jetzt kann ich raus!");
            startScript( S_EXIT_ROOM );
        }
    }
}

/*** Schreibtisch ***/
object OBJ_SCHREIBTISCH "Schreibtisch"
{
    verb vLook { sayLine(ACTOR_PLAYER,"Ein alter Holzschreibtisch."); }
}

/*** Schublade ***/
object OBJ_SCHUBLADE "Schublade"
{
    costume 3;            // obj003.png - closed drawer
    state  0  open=0;
    class  oClassDrawer;
    verb   vOpen
    {
        setState(OBJ_SCHUBLADE,1);
        sayLine(ACTOR_PLAYER,"Drin liegt eine Notiz.");
        putActorInRoom(OBJ_NOTIZ, ROOM_ID);
    }
}

/*** Notiz ***/
object OBJ_NOTIZ "Notiz"
{
    costume 4;            // obj004.png - paper note
    initialRoom  0;
    class  oClassPickup;
    verb vRead
    {
        sayLine(ACTOR_PLAYER,
          "Die Notiz sagt: ‘Der Schlüssel steckt im ROTEN Buch.’");
    }
}

/*** Bücherregal ***/
object OBJ_BUECHERREGAL "Bücherregal"
{
    verb vLook {
        sayLine(ACTOR_PLAYER,"Viele Bücher in verschiedenen Farben.");
    }
}

/*** Rotes Buch ***/
object OBJ_ROTES_BUCH "rotes Buch"
{
    costume 5;             // obj005.png - red book
    class oClassBook;
    verb vOpen
    {
        sayLine(ACTOR_PLAYER,"Da ist tatsächlich ein Schlüssel!");
        putActorInRoom(OBJ_SCHLUESSEL, ROOM_ID);
    }
}

/*** Schlüssel ***/
object OBJ_SCHLUESSEL "Schlüssel"
{
    costume 6;               // obj006.png - key
    initialRoom 0;
    class oClassPickup;
}

/*** Poster ***/
object OBJ_POSTER "Poster"
{
    verb vLook {
        sayLine(ACTOR_PLAYER,
            "Vier = 4, Neun = 9, Eins = 1 … hmm.");
    }
}

/*** Wörterbuch (inventory) ***/
object OBJ_WOERTERBUCH "Wörterbuch"
{
    costume 9;                // obj009.png - dictionary
    initialRoom 0;
    class oClassPickup;
}

/* ------------- Exit script ---------------- */
script S_EXIT_ROOM
{
    loadRoom(2);   // credits, next room, etc.
}

/* ------------------------------------------------------
   Custom classes (simplified wrappers for clarity)
   ------------------------------------------------------ */

class oClassPickup
{
    verb vPickUp
    {
        pickupObject(this);
        addToInventory(this);
        sayLine(ACTOR_PLAYER, "Okay.");
    }
}

class oClassBook
{
    verb vLook { sayLine(ACTOR_PLAYER,"Ein interessantes Buch."); }
}

/* …Door, Drawer, Box classes would pull from stdlib or expand here… */
