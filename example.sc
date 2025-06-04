/* ===============================================================
   Escape-Room: „Das Arbeitszimmer“          Language-learning demo
   Target: ScummC (https://github.com/ScummVM/scummc)
   =============================================================== */


/* main script - this is called automatically on game start */
script main()
{
    startRoom(Arbeitszimmer);  // start in the Arbeitszimmer
    startScript(Idle);  // keep the game running
}

room Arbeitszimmer
{
    image = "room1.png";  // Background image

    script entry()
    {
        //putActorAt(VAR_EGO, 296, 110, Arbeitszimmer);   // Not currently implemented
        //walkActorTo(VAR_EGO, 250, 110);  // Not currently implemented
    }

    /*** Tür ***/
    object ObjTuer
    {
        x = 516;
        y = 136;
        w = 402;
        h = 723;
        name = "Tür";

        verb vLook(int this, int that)
        {
            egoSay("Die Tür ist verschlossen.");
        }
    }

    /*** Tresor ***/
    object ObjTresor
    {
        x = 893;
        y = 703;
        w = 174;
        h = 184;
        name = "Tresor";
        states = {
            { 0, 0, "" },                   // state = 1
            { 0, 0, "tresor_code.png" },    // state = 2
            { 0, 0, "tresor_open.png" },    // state = 3
        };

        verb vLook(int this, int that)
        {
            egoSay("Ein massiver Tresor.");
        }

        verb   vOpen(int this, int that)
        {
            if (getState(this) >= 2) {
                egoSay("Er ist schon offen.");
            } else {
                egoSay("Ich brauche einen Schlüssel.");
            }
        }
        verb   vUse(int this, int that)
        {
            if (getState(this) == 1 && that == OBJ_SCHLUESSEL) {
                setState(this,2);          // unlock safe
                startScript( AskCode );
            } else if (getState(this) == 1) {
                egoSay("Das funktioniert nicht.");
            } else if (getState(this) == 2) {
                startScript( AskCode );  // ask for code input
            }
        }
    }

    /*** Schreibtisch ***/
    object OBJ_SCHREIBTISCH
    {
        x = 1;
        y = 599;
        w = 464;
        h = 321;
        name = "Schreibtisch";
        verb vLook(int this, int that) { egoSay("Ein alter Holzschreibtisch."); }
    }

    /*** Türgriff (appears only after safe opened) ***/
    object ObjTuergriff
    {
        x = 0;
        y = 0;
        w = 64;
        h = 64;
        name = "Türgriff";
        state = 0;  // initially hidden
        states = {
            { 0, 0, "tuergriff.png" }  // visible, after safe opened
        };

        class = { oClassPickup };  // can be picked up
        verb   vUse(int this, int that)
        {
            if (that == ObjTuer) {
                egoSay("Jetzt kann ich raus!");
                startRoom(Escaped);  // go to success screen
            }
        }
    }

    /*** Schublade ***/
    object OBJ_SCHUBLADE
    {
        x = 228;
        y = 681;
        w = 148;
        h = 70;
        name = "Schublade";
        states = {
            { 0, 0, "" },  // closed - no graphics, uses background image
            { 0, 0, "schublade_open.png" },     // open
        };

        verb vLook(int this, int that)
        {
            egoSay("Eine einfache Schublade.");
        }

        verb   vOpen(int this, int that)
        {
            setState(this,2);
            egoSay("Drin liegt eine Notiz.");
            setState(OBJ_NOTIZ, 1);  // make note visible
        }
    }

    /*** Notiz ***/
    object OBJ_NOTIZ
    {
        x = 0;
        y = 0;
        w = 100;
        h = 100;
        name = "Notiz";
        states = {
            { 1, 0, "notiz.png" }     // visible
        };
        state = 0;  // initially hidden

        class = { oClassPickup };
        verb vRead(int this, int that)
        {
            egoSay("Die Notiz sagt: ‘Der Schlüssel steckt im ROTEN Buch.’");
            setState(OBJ_ROTES_BUCH, 1);  // make red book interactable 
        }
    }

    /*** Bücherregal ***/
    object OBJ_BUECHERREGAL
    {
        x = 1049;
        y = 108;
        w = 394;
        h = 782;
        name = "Bücherregal";
        verb vLook(int this, int that) {
            egoSay("Viele Bücher in verschiedenen Farben.");
        }
    }

    /*** Rotes Buch ***/
    object OBJ_ROTES_BUCH
    {
        x = 1335;
        y = 355;
        w = 26;
        h = 126;
        name = "Rotes Buch";
        states = {
            { 0, 0, "" },  // visible
            { 0, 0, "" },  // open
        };
        state = 0;  // initially hidden; book is visible in background image but not interactable

        verb vOpen(int this, int that)
        {
            if (getState(this) != 1) {
                return;
            }

            setState(this, 2);  // open book
            egoSay("Da ist tatsächlich ein Schlüssel!");
            setState(OBJ_SCHLUESSEL, 1);  // make key visible
        }
    }

    /*** Schlüssel ***/
    object OBJ_SCHLUESSEL
    {
        x = 1335;
        y = 40;
        w = 50;
        h = 26;
        name = "Schlüssel";
        states = {
            { 0, 0, "schluessel.png" }  // visible
        };
        state = 0;  // initially hidden
        class = { oClassPickup };
    }
}


room Escaped
{
    image = "room2.webp";  // Background image for success screen

}

/* ------------- Idle script to keep the game running ------------- */
script Idle()
{
    while (1) {
        breakScript();     // keep script alive
    }
}

/*** Tresor-Panel (for code input) ***/
script AskCode()
{
    string code = prompt("Gib den 3-stelligen Code ein:");  // German prompt
    if (code == "491") {
        egoSay("Perfekt!");        // Correct
        setState(ObjTresor, 3);                  // completely open
        setState(ObjTuergriff, 1);  // reveal door handle
        startScript( S_REVEAL_HANDLE );
    } else {
        egoSay("Falsch…");
    }
}

script S_REVEAL_HANDLE()
{
    animateObject(ObjTuergriff, 1);   // small bounce animation
    egoSay("Ein Türgriff! Genau was ich brauche.");
}


class oClassPickup
{
    verb vPickUp(int this, int that)
    {
        pickupObject(this);
        egoSay("Okay.");
    }
}
