# EnvLib -- Tom Rettig's Environment Library for Visual FoxPro
The original version of EnvLib for Visual FoxPro was created by Tom Rettig in July 1995.

Tom Rettig passed away in February 1996.  Since then Visual FoxPro has moved forward significantly.

This 

EnvLib contains a collection of classes that you can use (and aggregate) to save and restore your environment. The classes save, set, and restore SET, ON, open table, system variable, and object properties.

## Example -- saving the current work area
In **standard Visual FoxPro**, saving the current work area looks something like this:

    LOCAL lcAlias
    lcAlias= SELECT()

    * Lots of code here

    * Now restore the previously selected work area
    SELECT ( lcAlias )   && Do this everywhere you might exit
    RETURN

With **Envlib**, you do this:

    LOCAL loAlias  
    loAlias= CREATEOBJECT( "SaveSelect" )

    * Lots of code here

    RETURN

Note you don't need to cover every exit point because when loAlias goes out of lexical scope, the work area is reset upon its destroy.

## Example -- saving the current work area and setting a new work area

    LOCAL lcAlias
    lcAlias= SELECT()
    SELECT Customer

    * Lots of code here

    * Now restore the previously selected work area
    SELECT ( lcAlias )   && Do this everywhere you might exit
    RETURN

With **Envlib**, you do this:

    LOCAL loAlias  
    loAlias= CREATEOBJECT( "SetSelect". "Customer" )

    * Lots of code here

    RETURN
