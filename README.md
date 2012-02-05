# EnvLib for Visual FoxPro 9 #

EnvLib is a collection of classes that you can use (and aggregate) to save and restore your environment. The classes save, set, and restore SET, ON, open table, system variable, object properties, and other helpers.

The original version of EnvLib for Visual FoxPro was created by **Tom Rettig** in July 1995. Tom passed away in February 1996.  Since then Visual FoxPro has moved forward significantly. This is an updated version of his original library.

# Features #
* Lightweight classes, one for each aspect of the Visual FoxPro environment.
* Objects save, and optionally set, aspects of the VFP environment upon `init()`.
* Objects restore their aspect of the VFP environment upon `destroy()`.
* **Therefore** facets of the VFP environment are automatically reset when objects go out of scope.
* Objects can be nested at design-time or run-time to group many aspects into one object.
* Objects can be configured at creation-time to not reset upon `destroy()`.

# Using EnvLib #
To include Envlib in your application, simply call `SET PROCEDURE ... ADDITIVE` prior to using it.

    SET PROCEDURE TO <path>\EnvLib ADDITIVE



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
