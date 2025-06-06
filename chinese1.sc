/* ===============================================================
   Escape‑Room: "The Study"          Language‑learning demo
   Target: ScummC (https://github.com/ScummVM/scummc)
   =============================================================== */

/* main script – called automatically on game start */
script main()
{
    startRoom(Arbeitszimmer);  // start in the study
    startScript(Idle);         // keep the game running
}

room Arbeitszimmer
{
    image = "room1-chinese.png";  // background image

    script entry()
    {
        //putActorAt(VAR_EGO, 296, 110, Arbeitszimmer);   // not implemented yet
        //walkActorTo(VAR_EGO, 250, 110);                  // not implemented yet
    }

    /*** Door ***/
    object ObjTuer
    {
        x = 516;
        y = 136;
        w = 402;
        h = 723;
        name = "门";

        verb vLook(int this, int that)
        {
            egoSay("门锁住了。");
        }
    }

    /*** Safe ***/
    object ObjTresor
    {
        x = 893;
        y = 703;
        w = 174;
        h = 184;
        name = "保险箱";
        states = [
            [ 0, 0, "" ],                   // state = 1 (locked)
            [ 0, 0, "tresor_code.png" ],    // state = 2 (unlocked, waiting code)
            [ 0, 0, "tresor_open.png" ],    // state = 3 (open)
        ];

        verb vLook(int this, int that)
        {
            egoSay("一个厚重的保险箱。");
        }

        verb vOpen(int this, int that)
        {
            if (getState(this) >= 2) {
                egoSay("它已经开着了。");
            } else {
                egoSay("我需要一把钥匙。");
            }
        }
        verb vUse(int this, int that)
        {
            if (getState(this) == 1 && that == OBJ_SCHLUESSEL) {
                setState(this,2);          // unlock safe with key
                startScript( AskCode );
            } else if (getState(this) == 1) {
                egoSay("这没用。");
            } else if (getState(this) == 2) {
                startScript( AskCode );    // ask for code input
            }
        }
    }

    /*** Desk ***/
    object OBJ_SCHREIBTISCH
    {
        x = 1;
        y = 599;
        w = 464;
        h = 321;
        name = "写字台";
        verb vLook(int this, int that) { egoSay("一张老旧的木制写字台。"); }
    }

    /*** Door handle (appears only after safe opened) ***/
    object ObjTuergriff
    {
        x = 930;
        y = 750;
        w = 96;
        h = 96;
        name = "门把手";
        state = 0;  // initially hidden
        states = [
            [ 0, 0, "tuergriff.png" ],  // visible once the safe is open
        ];

        class = [ oClassPickup ];  // can be picked up
        verb vUse(int this, int that)
        {
            if (that == ObjTuer) {
                egoSay("现在我可以出去了！");
                startRoom(Escaped);  // go to success screen
            }
        }
    }

    /*** Drawer ***/
    object OBJ_SCHUBLADE
    {
        x = 228;
        y = 681;
        w = 148;
        h = 70;
        name = "抽屉";
        states = [
            [ 0, 0, "" ],                    // closed – uses background graphic
            [ 0, 0, "schublade_open.png" ],  // open
        ];

        verb vLook(int this, int that)
        {
            egoSay("一个普通的抽屉。");
        }

        verb vOpen(int this, int that)
        {
            setState(this,2);
            egoSay("里面有一张便条。");
            setState(OBJ_NOTIZ, 1);  // reveal the note
        }
    }

    /*** Note ***/
    object OBJ_NOTIZ
    {
        x = 250;
        y = 681;
        w = 100;
        h = 100;
        name = "便条";
        states = [
            [ 1, 0, "notiz.png" ]     // visible
        ];
        state = 0;  // initially hidden

        class = [ oClassPickup ];
        verb vRead(int this, int that)
        {
            egoSay("便条写着：“钥匙在红色的书里。”");
            setState(OBJ_ROTES_BUCH, 1);  // make red book interactable
        }
    }

    /*** Bookshelf ***/
    object OBJ_BUECHERREGAL
    {
        x = 1049;
        y = 108;
        w = 394;
        h = 782;
        name = "书架";
        verb vLook(int this, int that) {
            egoSay("许多颜色各异的书。");
        }
    }

    /*** Red book ***/
    object OBJ_ROTES_BUCH
    {
        x = 1335;
        y = 355;
        w = 26;
        h = 126;
        name = "红色的书";
        states = [
            [ 0, 0, "" ],  // closed (visible in BG but not interactable)
            [ 0, 0, "" ],  // open
        ];
        state = 0;  // initially hidden

        verb vOpen(int this, int that)
        {
            if (getState(this) != 1) {
                return;
            }

            setState(this, 2);  // open the book
            egoSay("里面果然有一把钥匙！");
            setState(OBJ_SCHLUESSEL, 1);  // reveal key
        }
    }

    /*** Key ***/
    object OBJ_SCHLUESSEL
    {
        x = 1335;
        y = 355;
        w = 50;
        h = 26;
        name = "钥匙";
        states = [
            [ 0, 0, "schluessel.png" ]  // visible
        ];
        state = 0;  // initially hidden
        class = [ oClassPickup ];
    }
}

room Escaped
{
    image = "room2-chinese.png";  // background image for success screen
}

/* ------------- Idle script to keep the game running ------------- */
script Idle()
{
    while (1) {
        breakScript();  // keep script alive
    }
}

/*** Safe panel (code input) ***/
script AskCode()
{
    string code = prompt("请输入三位数密码：");  // Chinese prompt for learning
    if (code == "491") {
        egoSay("完美！");              // correct code
        setState(ObjTresor, 3);        // fully open
        setState(ObjTuergriff, 1);     // reveal door handle
        startScript( S_REVEAL_HANDLE );
    } else {
        egoSay("不对……");
    }
}

script S_REVEAL_HANDLE()
{
    animateObject(ObjTuergriff, 1);   // small bounce animation
    egoSay("一个门把手！正是我需要的。");
}

class oClassPickup
{
    verb vPickUp(int this, int that)
    {
        pickupObject(this);
        egoSay("好的。");
    }
}
