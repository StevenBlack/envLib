# EnvLib for Visual FoxPro 9 #

EnvLib is a collection of classes that you can use (and aggregate) to save and restore your environment. The classes save, set, and restore SET, ON, open table, system variable, object properties, and other helpers.

The original version of EnvLib for Visual FoxPro was created by **Tom Rettig** in July 1995. Tom passed away in February 1996.  Since then Visual FoxPro has moved forward significantly. This is an updated version of his original library.


# Features #
* Over 120 lightweight classes, one for each aspect of the Visual FoxPro environment.
* Objects save, and optionally set, aspects of the VFP environment upon `init()`.
* Objects restore their aspect of the VFP environment upon `destroy()`.
* **Therefore** facets of the VFP environment are automatically reset when objects go out of scope.
* Objects can be nested at design-time or run-time to group many aspects into one object.
* Objects can be configured at creation-time to not reset upon `destroy()`.

# Using EnvLib #
To include Envlib in your application, simply call `SET PROCEDURE ... ADDITIVE` prior to using it.

    SET PROCEDURE TO <path>\EnvLib ADDITIVE


## Example -- saving the current path
In **standard Visual FoxPro**, saving and setting `SET PATH` looks something like this:

    LOCAL lcAPath
    lcPath= SET( "path" )
    SET PATH TO newPath  && or SET PATH TO newPath ADDITIVE

    * Lots of code here

    * Now restore the previously selected work area
    SET PATH TO &lcPath   && Do this everywhere you might exit
    RETURN

With **Envlib**, you do this:

    LOCAL loAlias  
    loAlias= CREATEOBJECT( "SetPath", "newPath" )
	* or loAlias= CREATEOBJECT( "SetPath", "newPath", "Additive" )

    * Lots of code here

    RETURN


Note you don't need to cover every exit point because when `lcPath` goes out of lexical scope, `SET PATH` is reset upon its destroy.


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

## Classes in EnvLib

### Abstract Classes
<table>
	<tr><th>Class</th><th>Parent</th><th>Notes</th></tr>
	<tr><td>Set</td><td>Custom</td><td>Abstract class </td></tr>
	<tr><td>SetTwo</td><td>Set </td><td>Abstract class</td></tr>
	<tr><td>SetOnOff</td><td>Set </td><td>Abstract class</td></tr>
	<tr><td>SetOnOffTwo</td><td>SetTwo </td><td>Abstract class</td></tr>
	<tr><td>On</td><td>Custom</td><td>Abstract class</td></tr>
	<tr><td>SaveArea</td><td>Custom</td><td>Abstract class</td></tr>
	<tr><td>SaveUsedArea</td><td>SaveArea</td><td>Abstract class</td></tr>
</table>

### SET Classes
These classes save, set, and restore the various VFP `SET` commands that have corresponding `SET()` functions.
<table>
	<tr><th>Class</th><th>Parent</th></tr>
	<tr><td>SetAlternate</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetAnsi</td><td>SetOnOff</td></tr>
	<tr><td>SetAsserts</td><td>SetOnOff</td></tr>
	<tr><td>SetAutoIncError</td><td>SetOnOff</td></tr>
	<tr><td>SetAutosave</td><td>SetOnOff</td></tr>
	<tr><td>SetBell</td><td>SetOnOff</td></tr>
	<tr><td>SetBlocksize</td><td>Set</td></tr>
	<tr><td>SetBrstatus</td><td>SetOnOff</td></tr>
	<tr><td>SetCarry</td><td>SetOnOff</td></tr>
	<tr><td>SetCentury</td><td>SetOnOff</td></tr>
	<tr><td>SetClassLib</td><td>Set</td></tr>
	<tr><td>SetClear</td><td>SetOnOff</td></tr>
	<tr><td>SetClock</td><td>SetOnOff</td></tr>
	<tr><td>SetCollate</td><td>Set</td></tr>
	<tr><td>SetCoverage</td><td>Set</td></tr>
	<tr><td>SetColor</td><td>Set</td></tr>
	<tr><td>SetCompatible</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetConfirm</td><td>SetOnOff</td></tr>
	<tr><td>SetConsole</td><td>SetOnOff</td></tr>
	<tr><td>SetCpcompile</td><td>Set</td></tr>
	<tr><td>SetCpdialog</td><td>SetOnOff</td></tr>
	<tr><td>SetCurrency</td><td>SetTwo</td></tr>
	<tr><td>SetCursor</td><td>SetOnOff</td></tr>
	<tr><td>SetDatabase</td><td>Set</td></tr>
	<tr><td>SetDataSession</td><td>Set</td></tr>
	<tr><td>SetDate</td><td>Set</td></tr>
	<tr><td>SetDebug</td><td>SetOnOff</td></tr>
	<tr><td>SetDecimals</td><td>Set</td></tr>
	<tr><td>SetDefault</td><td>Set</td></tr>
	<tr><td>SetDeleted</td><td>SetOnOff</td></tr>
	<tr><td>SetDelimiters</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetDevelopment</td><td>SetOnOff</td></tr>
	<tr><td>SetDisplay</td><td>Set</td></tr>
	<tr><td>SetDohistory</td><td>SetOnOff</td></tr>
	<tr><td>SetEcho</td><td>SetOnOff</td></tr>
	<tr><td>SetEngineBehavior</td><td>Set</td></tr>
	<tr><td>SetEscape</td><td>SetOnOff</td></tr>
	<tr><td>SetExact</td><td>SetOnOff</td></tr>
	<tr><td>SetExclusive</td><td>SetOnOff</td></tr>
	<tr><td>SetFdow</td><td>Set</td></tr>
	<tr><td>SetFixed</td><td>SetOnOff</td></tr>
	<tr><td>SetFullPath</td><td>SetOnOff</td></tr>
	<tr><td>SetFweek</td><td>Set</td></tr>
	<tr><td>SetHeadings</td><td>SetOnOff</td></tr>
	<tr><td>SetHelp</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetHelpfilter</td><td>Set</td></tr>
	<tr><td>SetHours</td><td>Set</td></tr>
	<tr><td>SetIntensity</td><td>SetOnOff</td></tr>
	<tr><td>SetKeycomp</td><td>Set</td></tr>
	<tr><td>SetLibrary</td><td>Set</td></tr>
	<tr><td>SetLock</td><td>SetOnOff</td></tr>
	<tr><td>SetLogErrors</td><td>SetOnOff</td></tr>
	<tr><td>SetMargin</td><td>Set</td></tr>
	<tr><td>SetMackey</td><td>Set</td></tr>
	<tr><td>SetMark</td><td>Set</td></tr>
	<tr><td>SetMemoWidth</td><td>Set</td></tr>
	<tr><td>SetMessage</td><td>SetTwo</td></tr>
	<tr><td>SetMultiLocks</td><td>SetOnOff</td></tr>
	<tr><td>SetNear</td><td>SetOnOff</td></tr>
	<tr><td>SetNotify</td><td>SetOnOff</td></tr>
	<tr><td>SetNull</td><td>SetOnOff</td></tr>
	<tr><td>SetNullDisplay</td><td>Set</td></tr>
	<tr><td>SetOdometer</td><td>Set</td></tr>
	<tr><td>SetOLEObject</td><td>SetOnOff</td></tr>
	<tr><td>SetOptimize</td><td>SetOnOff</td></tr>
	<tr><td>SetPalette</td><td>SetOnOff</td></tr>
	<tr><td>SetPath</td><td>Set</td></tr>
	<tr><td>SetPrinter</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetPoint</td><td>Set</td></tr>
	<tr><td>SetProcedure</td><td>Set</td></tr>
	<tr><td>SetReadBorder</td><td>SetOnOff</td></tr>
	<tr><td>SetRefresh</td><td>SetTwo</td></tr>
	<tr><td>SetReprocess</td><td>Set</td></tr>
	<tr><td>SetResource</td><td>SetOnOffTwo</td></tr>
	<tr><td>SetResourceCreate</td><td>SetResource</td></tr>
	<tr><td>SetSafety</td><td>SetOnOff</td></tr>
	<tr><td>SetSeconds</td><td>SetOnOff</td></tr>
	<tr><td>SetSeparator</td><td>Set</td></tr>
	<tr><td>SetSpace</td><td>SetOnOff</td></tr>
	<tr><td>SetStatus</td><td>SetOnOff</td></tr>
	<tr><td>SetStatusBar</td><td>SetOnOff</td></tr>
	<tr><td>SetStep</td><td>SetOnOff</td></tr>
	<tr><td>SetSysFormats</td><td>SetOnOff</td></tr>
	<tr><td>SetSysMenu</td><td>Set</td></tr>
	<tr><td>SetTableValidate</td><td>Set</td></tr>
	<tr><td>SetTalk</td><td>SetOnOff</td></tr>
	<tr><td>SetTopic</td><td>SetTwo</td></tr>
	<tr><td>SetTrBetween</td><td>SetOnOff</td></tr>
	<tr><td>SetTypeahead</td><td>Set</td></tr>
	<tr><td>SetUdfParms</td><td>Set</td></tr>
	<tr><td>SetUnique</td><td>SetOnOff</td></tr>
	<tr><td>SetView</td><td>SetOnOff</td></tr>
	<tr><td>SetWindowOfMemo</td><td>Set</td></tr>
	<tr><td>SetVfpDefaults</td><td>Custom</td></tr>
</table>

### ON Classes
These classes save, set, and restore the various VFP `ON` commands that have corresponding `ON()` functions.
<table>
	<tr><th>Class</th><th>Parent</th></tr>
	<tr><td>OnError</td><td>On</td></tr>
	<tr><td>OnKey</td><td>On</td></tr>
	<tr><td>OnKeyLabel</td><td>On</td></tr>
	<tr><td>OnShutDown</td><td>On</td></tr>
</table>

### Workarea Helper Classes
These classes save, set, and restore the various aspects of saving the properties of work areas.
<table>
	<tr><th>Class</th><th>Parent</th></tr>
	<tr><td>SaveSelect</td><td>SaveArea</td></tr>
	<tr><td>SetSelect</td><td>SaveSelect</td></tr>
	<tr><td>SaveBuffering</td><td>SaveUsedArea</td></tr>
	<tr><td>SetBuffering</td><td>SaveBuffering</td></tr>
	<tr><td>SaveRecno</td><td>SaveUsedArea</td></tr>
	<tr><td>SaveOrder</td><td>SaveUsedArea</td></tr>
	<tr><td>SetOrder</td><td>SaveOrder</td></tr>
	<tr><td>SaveFilter</td><td>SaveUsedArea</td></tr>
	<tr><td>SetFilter</td><td>SaveFilter</td></tr>
	<tr><td>SaveRelation</td><td>SaveUsedArea</td></tr>
	<tr><td>SetRelation</td><td>SaveRelation</td></tr>
	<tr><td>SaveTable</td><td>SaveUsedArea</td></tr>
	<tr><td>SaveAllTables</td><td>Custom</td></tr>
	<tr><td>OpenAliasCheckpoint</td><td>Custom</td></tr>
</table>

### Miscellaneous Helper Classes
These classes save, set, and restore various other things.
<table>
	<tr><th>Class</th><th>Parent</th><th>Notes</th></tr>
	<tr><td>SaveProperty</td><td>Custom</td><td></td></tr>
	<tr><td>SetProperty</td><td>SaveProperty</td><td></td></tr>
	<tr><td>SetSysVar</td><td>Custom</td><td></td></tr>
	<tr><td>MessageTimer</td><td>Timer</td><td></td></tr>
	<tr><td>SetLockScreen</td><td>Custom</td><td>Saves and sets <code>form.lockScreen</code>, resetting it when the instance goes out of scope.</td></tr>
	<tr><td>ScopeTimer</td><td>Custom</td><td>Upon <code>destroy()</code> debugout the object's scope lifetime in seconds..</td></tr>
</table>
