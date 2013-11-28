* Program: EnvLib.prg
* Classes: Many definitions, see below.
*   Bases: All abstract classes are based on Custom.
*  Notice: The author releases all rights to the public domain
*        : subject to the Warranty Disclaimer below.
*  Author: Tom Rettig (1941-1996)
*        : Rettig Micro Corporation
*        : First released (Version 1.0) July 15, 1995
* Updates: Steven Black
*        : Steven Black Consulting
*        : http://stevenblack.com
* Version: EnvLib for VFP 9, November 27, 2013
*  Action: Save, set, and restore SET, ON, open table, system variable,
*        :    and object property environments.
*Requires: Visual FoxPro for Windows version 3.0 or later
*   Notes: - May be freely used, modified, and distributed in
*        : compiled and/or source code form.
*        : - The author appreciates acknowledgment in commercial
*        : products and publications that use or learn from this class.
*        : - Technical support is not officially provided.  The
*        : author and maintainers are interested in hearing about problems
*        : or enhancement requests you have, and will try to be
*        : helpful within reasonable limits.  
*        : - Warranty Disclaimer: NO WARRANTY!!!
*        : THE AUTHOR RELEASES TO THE PUBLIC DOMAIN ALL CLAIMS TO ANY
*        : RIGHTS IN THIS PROGRAM AND FREELY PROVIDES IT ÒAS ISÓ WITHOUT
*        : WARRANTY OF ANY KIND, EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
*        : LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
*        : FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE AUTHOR, OR ANY
*        : OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THIS PROGRAM, BE
*        : LIABLE FOR ANY COMMERCIAL, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
*        : DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM
*        : INCLUDING, BUT NOT LIMITED TO, LOSS OF DATA OR DATA BEING
*        : RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR LOSSES
*        : SUSTAINED BY THIRD PARTIES OR A FAILURE OF THE PROGRAM TO
*        : OPERATE WITH ANY OTHER PROGRAMS, EVEN IF YOU OR OTHER PARTIES
*        : HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

*************************************************************
* SET Parent Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS Set AS Custom  && abstract class
*------------------------------------------------------------------------------
   PROTECTED uDefault, ;
             uOldSet, ;
             uNewSet, ;
             lNoReset

   FUNCTION GetOld
      RETURN This.uOldSet

   FUNCTION GetNew
      RETURN This.uNewSet

   FUNCTION GetDefault
      RETURN This.uDefault

   PROTECTED PROCEDURE Init( tcSet, tuValue )
      This.uOldSet = SET( tcSet )
      This.uNewSet = NVL( tuValue, This.uDefault )
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTwo AS Set   && abstract class
*------------------------------------------------------------------------------
   PROTECTED uDefaultTwo, ;
             uOldSetTwo, ;
             uNewSetTwo, ;
             cSet

   FUNCTION GetOldTwo
      RETURN This.uOldSetTwo

   FUNCTION GetNewTwo
      RETURN This.uNewSetTwo

   FUNCTION GetDefaultTwo
      RETURN This.uDefaultTwo

   PROTECTED PROCEDURE Init( tcSet, tuValueOne, ;
                            tuValueTwo, tnParams )
      DO CASE  && of which to set
         CASE EMPTY( tnParams )
            ERROR 11  &&  was: cnVF_ERR_PARAM_INVALID
            RETURN .F.  && early exit
         CASE tnParams == 1
            This.cSet = "1"
         CASE EMPTY( tuValueOne )  && never a valid value
            This.cSet = "2"
         OTHERWISE
            This.cSet = "3"
      ENDCASE  && of which to set

      * Primary value as returned by SET( "whatever" ).
      IF INLIST( This.cSet, "1", "3" )
         =DODEFAULT( tcSet, tuValueOne )
      ENDIF

      * Secondary value as returned by SET( "whatever", 1 ).
      IF INLIST( This.cSet, "2", "3" )
         This.uOldSetTwo = SET( tcSet, 1 )
         This.uNewSetTwo = NVL( tuValueTwo, This.uDefaultTwo )
      ENDIF
ENDDEFINE

*------------------------------------------------------------------------------
DEFINE CLASS SetOnOff AS Set   && abstract class
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcSet, tcValue )
      DO CASE
         CASE ISNULL( tcValue )
            =DODEFAULT( tcSet, tcValue )
         CASE NOT INLIST( UPPER( ALLTRIM( tcValue )), "ON", "OFF" )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            =DODEFAULT( tcSet, UPPER( ALLTRIM( tcValue )) )
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetOnOffTwo AS SetTwo   && abstract class
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcSet, ;
                            tcValueOne, ;
                            tuValueTwo, ;
                            tnParams )
      DO CASE
         CASE ISNULL( tcValueOne )
            =DODEFAULT( tcSet, ;
                          tcValueOne, ;
                          tuValueTwo, ;
                          tnParams )
         CASE NOT INLIST( UPPER( ALLTRIM( tcValueOne )), "ON", "OFF" )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            =DODEFAULT( tcSet, ;
                          UPPER( ALLTRIM( tcValueOne )), ;
                          tuValueTwo, ;
                          tnParams )
      ENDCASE
ENDDEFINE


*************************************************************
* SET Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SetAlternate AS SetOnOffTwo
*------------------------------------------------------------------------------
   uDefault    = "OFF"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init( tcOnOff, tcTo, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "ALTERNATE", ;
                                    tcOnOff, tcTo, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET ALTERNATE ON
         OTHERWISE
            SET ALTERNATE OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST( This.cSet, "2", "3" )
            * Do nothing.
         CASE EMPTY( This.uNewSetTwo )
            SET ALTERNATE TO
         CASE ( NOT EMPTY( tcOption )) AND;
              UPPER( ALLTRIM( tcOption )) == "ADDITIVE"
            SET ALTERNATE TO ( This.uNewSetTwo ) ADDITIVE
            IF This.uNewSet == "ON"
               SET ALTERNATE ON
            ENDIF
         OTHERWISE
            SET ALTERNATE TO ( This.uNewSetTwo )
            IF This.uNewSet == "ON"
               SET ALTERNATE ON
            ENDIF
      ENDCASE  && of secondary set

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET ALTERNATE ON
            OTHERWISE
               SET ALTERNATE OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE EMPTY( This.uOldSetTwo )
               SET ALTERNATE TO
            OTHERWISE
               SET ALTERNATE TO ( This.uOldSetTwo )
         ENDCASE  && of secondary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetAnsi AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "ANSI", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET ANSI ON
         OTHERWISE
            SET ANSI OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET ANSI ON
         OTHERWISE
            SET ANSI OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetAsserts AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "Asserts", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET Asserts ON
         OTHERWISE
            SET Asserts OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET Asserts ON
         OTHERWISE
            SET Asserts OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetAutoIncError AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "AutoIncError", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET AutoIncError ON
         OTHERWISE
            SET AutoIncError OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET AutoIncError ON
         OTHERWISE
            SET AutoIncError OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetAutosave AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "AUTOSAVE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET AUTOSAVE ON
         OTHERWISE
            SET AUTOSAVE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET AUTOSAVE ON
         OTHERWISE
            SET AUTOSAVE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetBell AS SetOnOff
*------------------------------------------------------------------------------
   * Limit - no way to get SET BELL TO <freq|.wav>, <sec>
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "BELL", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET BELL ON
         OTHERWISE
            SET BELL OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET BELL ON
         OTHERWISE
            SET BELL OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetBlocksize AS Set
*------------------------------------------------------------------------------
   uDefault = 64

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "BLOCKSIZE", tnValue )
         SET BLOCKSIZE TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET BLOCKSIZE TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetBrstatus AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "BRSTATUS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET BRSTATUS ON
         OTHERWISE
            SET BRSTATUS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET BRSTATUS ON
         OTHERWISE
            SET BRSTATUS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCarry AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CARRY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CARRY ON
         OTHERWISE
            SET CARRY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CARRY ON
         OTHERWISE
            SET CARRY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCentury AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CENTURY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CENTURY ON
         OTHERWISE
            SET CENTURY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CENTURY ON
         OTHERWISE
            SET CENTURY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetClassLib AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "CLASSLIB", tcValue )
         LOCAL lcTemp
         lcTemp = This.uNewSet
         IF ( NOT EMPTY( tcOption )) AND;
            ( UPPER( ALLTRIM( tcOption ))="ADDITIVE" )
            SET CLASSLIB TO &lcTemp ADDITIVE
         ELSE
            SET CLASSLIB TO &lcTemp
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         LOCAL lcTemp
         lcTemp = This.uOldSet
         SET CLASSLIB TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetClear AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CLEAR", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CLEAR ON
         OTHERWISE
            SET CLEAR OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CLEAR ON
         OTHERWISE
            SET CLEAR OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetClock AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CLOCK", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CLOCK ON
         CASE This.uNewSet == "STATUS"
            SET CLOCK STATUS
         OTHERWISE
            SET CLOCK OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CLOCK ON
         CASE This.uOldSet == "STATUS"
            SET CLOCK STATUS
         OTHERWISE
            SET CLOCK OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCollate AS Set
*------------------------------------------------------------------------------
   uDefault = "MACHINE"

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "COLLATE", tnValue )
         SET COLLATE TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET COLLATE TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCoverage AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "Coverage", tcValue )
         LOCAL lcTemp
         lcTemp = This.uNewSet
         IF ( NOT EMPTY( tcOption )) AND;
            ( UPPER( ALLTRIM( tcOption ))="ADDITIVE" )
            SET Coverage TO &lcTemp ADDITIVE
         ELSE
            SET Coverage TO &lcTemp
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         LOCAL lcTemp
         lcTemp = This.uOldSet
         SET Coverage TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetColor AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "COLOR", tcValue )
         SET COLOR TO ( This.uNewSet )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET COLOR TO ( This.uOldSet )
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCompatible AS SetOnOffTwo
*------------------------------------------------------------------------------
   uDefault    = "OFF"
   uDefaultTwo = "PROMPT"

   PROTECTED PROCEDURE Init( tcOnOff, tcPrompt, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      LOCAL lcOnOff, lcPrompt
      lcOnOff  = IIF( ISNULL( tcOnOff ), tcOnOff, UPPER( ALLTRIM( tcOnOff )) )
      lcPrompt = IIF( ISNULL( tcPrompt ), tcPrompt, UPPER( ALLTRIM( tcPrompt )) )
      DO CASE
         CASE PARAMETERS() > 1 AND EMPTY( tcOnOff )
            lcOnOff = SET( "COMPATIBLE" )
         CASE lcOnOff == "FOXPLUS"
            lcOnOff = "OFF"
         CASE lcOnOff == "DB4"
            lcOnOff = "ON"
      ENDCASE

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "COMPATIBLE", ;
                                    lcOnOff, lcPrompt, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT This.cSet == "1"
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET COMPATIBLE ON
         OTHERWISE
            SET COMPATIBLE OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST( This.cSet, "2", "3" )
            * Do nothing.
         CASE This.uNewSetTwo == "PROMPT"
            IF This.uNewSet == "ON"
               SET COMPATIBLE ON PROMPT
            ELSE
               SET COMPATIBLE OFF PROMPT
            ENDIF
         CASE This.uNewSetTwo == "NOPROMPT"
            IF This.uNewSet == "ON"
               SET COMPATIBLE ON NOPROMPT
            ELSE
               SET COMPATIBLE OFF NOPROMPT
            ENDIF
         OTHERWISE
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
      ENDCASE  && of secondary set

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT This.cSet == "1"
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET COMPATIBLE ON
            OTHERWISE
               SET COMPATIBLE OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE This.uOldSetTwo == "NOPROMPT"
               IF This.uOldSet == "ON"
                  SET COMPATIBLE ON NOPROMPT
               ELSE
                  SET COMPATIBLE OFF NOPROMPT
               ENDIF
            OTHERWISE
               IF This.uOldSet == "ON"
                  SET COMPATIBLE ON PROMPT
               ELSE
                  SET COMPATIBLE OFF PROMPT
               ENDIF
         ENDCASE  && of secondary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetConfirm AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CONFIRM", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CONFIRM ON
         OTHERWISE
            SET CONFIRM OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CONFIRM ON
         OTHERWISE
            SET CONFIRM OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetConsole AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CONSOLE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CONSOLE ON
         OTHERWISE
            SET CONSOLE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CONSOLE ON
         OTHERWISE
            SET CONSOLE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCpcompile AS Set
*------------------------------------------------------------------------------
   uDefault = CPCURRENT()

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "CPCOMPILE", tnValue )
         SET CPCOMPILE TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET CPCOMPILE TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCpdialog AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CPDIALOG", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CPDIALOG ON
         OTHERWISE
            SET CPDIALOG OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CPDIALOG ON
         OTHERWISE
            SET CPDIALOG OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCurrency AS SetTwo
*------------------------------------------------------------------------------
   uDefault    = "LEFT"
   uDefaultTwo = "$"

   PROTECTED PROCEDURE Init( tcLeftRight, tcTo, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "CURRENCY", ;
                               IIF( ISNULL( tcLeftRight ), ;
                                   tcLeftRight, ;
                                   UPPER( ALLTRIM( tcLeftRight )) ), ;
                               tcTo, ;
                               PARAMETERS() )
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE NOT INLIST( This.uNewSet, "LEFT", "RIGHT" )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE This.uNewSet == "LEFT"
            SET CURRENCY LEFT
         OTHERWISE
            SET CURRENCY RIGHT
      ENDCASE  && of primary set

      * Secondary set.
      IF INLIST( This.cSet, "2", "3" )
         SET CURRENCY TO ( This.uNewSetTwo )
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "LEFT"
               SET CURRENCY LEFT
            OTHERWISE
               SET CURRENCY RIGHT
         ENDCASE  && of primary set

         * Secondary set.
         IF INLIST( This.cSet, "2", "3" )
            SET CURRENCY TO ( This.uOldSetTwo )
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetCursor AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "CURSOR", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET CURSOR ON
         OTHERWISE
            SET CURSOR OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET CURSOR ON
         OTHERWISE
            SET CURSOR OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDatabase AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "DATABASE", tcValue )
            RETURN .F.  && early exit
         CASE EMPTY( This.uNewSet )
            SET DATABASE TO
         OTHERWISE
            SET DATABASE TO ( This.uNewSet )
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE EMPTY( This.uOldSet )
            SET DATABASE TO
         OTHERWISE
            SET DATABASE TO ( This.uOldSet )
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDataSession AS Set
*------------------------------------------------------------------------------
   uDefault = 1

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "DATASESSION", tnValue )
         IF NOT EMPTY( tnValue )
            SET DATASESSION TO This.uNewSet
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET DATASESSION TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDate AS Set
*------------------------------------------------------------------------------
   uDefault = "AMERICAN"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "DATE", tcValue )
         SET DATE TO ( This.uNewSet )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET DATE TO ( This.uOldSet )
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDebug AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "DEBUG", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET DEBUG ON
         OTHERWISE
            SET DEBUG OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET DEBUG ON
         OTHERWISE
            SET DEBUG OFF
      ENDCASE
ENDDEFINE

*------------------------------------------------------------------------------
DEFINE CLASS SetDebugout AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "Debugout", tcValue )
         LOCAL lcTemp
         lcTemp = This.uNewSet
         IF ( NOT EMPTY( tcOption )) AND;
            ( UPPER( ALLTRIM( tcOption ))="ADDITIVE" )
            SET Debugout TO &lcTemp ADDITIVE
         ELSE
            SET Debugout TO &lcTemp
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         LOCAL lcTemp
         lcTemp = This.uOldSet
         SET Debugout TO &lcTemp
      ENDIF
ENDDEFINE

*------------------------------------------------------------------------------
DEFINE CLASS SetDecimals AS Set
*------------------------------------------------------------------------------
   uDefault = 2

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "DECIMALS", tnValue )
         SET DECIMALS TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET DECIMALS TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDefault AS Set
*------------------------------------------------------------------------------
   uDefault = SYS( 5 )+CURDIR()

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .F.   && Note: this is different than some other classes here.
      ENDIF

      This.uOldSet = SYS( 5 )+CURDIR()
      This.uNewSet = EVL( tcValue, This.uDefault )
      cd ( This.uNewSet )

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         cd ( This.uOldSet )
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDeleted AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "DELETED", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET DELETED ON
         OTHERWISE
            SET DELETED OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET DELETED ON
         OTHERWISE
            SET DELETED OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDelimiters AS SetOnOffTwo
*------------------------------------------------------------------------------
   uDefault    = "OFF"
   uDefaultTwo = ":"

   PROTECTED PROCEDURE Init( tcOnOff, tcDelimiter, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      LOCAL lcOnOff, lcDelimiter
      lcOnOff  = IIF( ISNULL( tcOnOff ), tcOnOff, UPPER( ALLTRIM( tcOnOff )) )
      lcDelimiter = NVL( tcDelimiter, "" )
      IF PARAMETERS() > 1 AND EMPTY( tcOnOff )
         lcOnOff = SET( "DELIMITERS" )
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "DELIMITERS", ;
                                    lcOnOff, lcDelimiter, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT This.cSet == "1"
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET DELIMITERS ON
         OTHERWISE
            SET DELIMITERS OFF
      ENDCASE  && of primary set

      IF NOT EMPTY( lcDelimiter )
         SET DELIMITERS TO lcDelimiter
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset

         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET DELIMITERS ON
            OTHERWISE
               SET DELIMITERS OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE NOT EMPTY( This.uOldSetTwo )
               SET DELIMITERS TO ( This.uOldSetTwo )
            OTHERWISE
               IF This.uOldSet == "ON"
                  SET DELIMITERS ON
               ELSE
                  SET DELIMITERS OFF
               ENDIF
         ENDCASE  && of secondary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDevelopment AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "DEVELOPMENT", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET DEVELOPMENT ON
         OTHERWISE
            SET DEVELOPMENT OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET DEVELOPMENT ON
         OTHERWISE
            SET DEVELOPMENT OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDisplay AS Set
*------------------------------------------------------------------------------
   uDefault = "VGA25"

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "DISPLAY", tnValue )
         lcTemp = This.uNewSet
         SET DISPLAY TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         lcTemp = This.uOldSet
         SET DISPLAY TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetDohistory AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "DOHISTORY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET DOHISTORY ON
         OTHERWISE
            SET DOHISTORY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET DOHISTORY ON
         OTHERWISE
            SET DOHISTORY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetEcho AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "ECHO", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET ECHO ON
         OTHERWISE
            * Must RELEASE WINDOW TRACE to set ECHO OFF
            RELEASE WINDOW TRACE
            SET ECHO OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET ECHO ON
         OTHERWISE
            * Must RELEASE WINDOW TRACE to set ECHO OFF
            RELEASE WINDOW TRACE
            SET ECHO OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetEngineBehavior AS Set
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "EngineBehavior", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "70"
            SET EngineBehavior 70
         CASE This.uNewSet == "80"
            SET EngineBehavior 80
         CASE This.uNewSet == "90"
            SET EngineBehavior 90
         OTHERWISE
            SET EngineBehavior 90
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "70"
            SET EngineBehavior 70
         CASE This.uOldSet == "80"
            SET EngineBehavior 80
         CASE This.uOldSet == "90"
            SET EngineBehavior 90
         OTHERWISE
            SET EngineBehavior 90
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetEscape AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "ESCAPE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET ESCAPE ON
         OTHERWISE
            SET ESCAPE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET ESCAPE ON
         OTHERWISE
            SET ESCAPE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetExact AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "EXACT", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET EXACT ON
         OTHERWISE
            SET EXACT OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET EXACT ON
         OTHERWISE
            SET EXACT OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetExclusive AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "EXCLUSIVE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET EXCLUSIVE ON
         OTHERWISE
            SET EXCLUSIVE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET EXCLUSIVE ON
         OTHERWISE
            SET EXCLUSIVE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetFdow AS Set
*------------------------------------------------------------------------------
   uDefault = 1

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "FDOW", tnValue )
         SET FDOW TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET FDOW TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetFixed AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "FIXED", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET FIXED ON
         OTHERWISE
            SET FIXED OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET FIXED ON
         OTHERWISE
            SET FIXED OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetFullPath AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "FULLPATH", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET FULLPATH ON
         OTHERWISE
            SET FULLPATH OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET FULLPATH ON
         OTHERWISE
            SET FULLPATH OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetFweek AS Set
*------------------------------------------------------------------------------
   uDefault = 1

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "FWEEK", tnValue )
         SET FWEEK TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET FWEEK TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetHeadings AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "HEADINGS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET HEADINGS ON
         OTHERWISE
            SET HEADINGS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET HEADINGS ON
         OTHERWISE
            SET HEADINGS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetHelp AS SetOnOffTwo
*------------------------------------------------------------------------------
   uDefault    = "ON"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init( tcOnOff, tcTo, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "HELP", ;
                                    tcOnOff, tcTo, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET HELP ON
         OTHERWISE
            SET HELP OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST( This.cSet, "2", "3" )
            * Do nothing.
         CASE EMPTY( This.uNewSetTwo )
            SET HELP TO
         OTHERWISE
            SET HELP TO ( This.uNewSetTwo )
      ENDCASE  && of secondary set

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET HELP ON
            OTHERWISE
               SET HELP OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE EMPTY( This.uOldSetTwo )
               SET HELP TO
            OTHERWISE
               SET HELP TO ( This.uOldSetTwo )
         ENDCASE  && of secondary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetHelpfilter AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "HELPFILTER", tcValue )
         LOCAL lcTemp
         lcTemp = This.uNewSet
         SET HELPFILTER TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         LOCAL lcTemp
         lcTemp = This.uOldSet
         SET HELPFILTER TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetHours AS Set
*------------------------------------------------------------------------------
   uDefault = 12

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "HOURS", tnValue )
            * No op?
         CASE ISNULL( This.uNewSet ) OR EMPTY( This.uNewSet )
            SET HOURS TO  && will default to 12
         * SET HOURS ignores decimals, i.e. 12.5 is legal
         CASE NOT TYPE( "This.uNewSet" )="N" OR ;
            NOT INLIST( INT( This.uNewSet ), 12, 24 )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            SET HOURS TO This.uNewSet
      ENDCASE

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         * SET( "hours" ) can only return 12 or 24 - never EMPTY()
         SET HOURS TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetIntensity AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "INTENSITY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET INTENSITY ON
         OTHERWISE
            SET INTENSITY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET INTENSITY ON
         OTHERWISE
            SET INTENSITY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetKeycomp AS Set
*------------------------------------------------------------------------------
   * Cannot initialize uDefault in the class body because DO CASE
   * logic is invalid here.  Done at start of Init instead.

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      DO CASE
         CASE _WINDOWS
            This.uDefault = "WINDOWS"
         CASE _MAC
            This.uDefault = "MAC"
         CASE _DOS
            This.uDefault = "DOS"
         OTHERWISE  && should never happen
            ERROR "Unknown operating system"
      ENDCASE
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "KEYCOMP", tcValue )
            RETURN .F.
         CASE NOT INLIST( UPPER( This.uNewSet ), "DOS", "WIND", ;
               "WINDO", "WINDOW", "WINDOWS" )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE "DOS" $ This.uNewSet
            SET KEYCOMP TO DOS
         OTHERWISE
            SET KEYCOMP TO WINDOWS
      ENDCASE

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         IF "DOS" $ This.uOldSet
            SET KEYCOMP TO DOS
         ELSE
            SET KEYCOMP TO WINDOWS
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetLibrary AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "LIBRARY", tcValue )
         lcTemp = This.uNewSet
         IF ( NOT EMPTY( tcOption )) AND;
            ( UPPER( ALLTRIM( tcOption ))="ADDITIVE" )
            SET LIBRARY TO &lcTemp ADDITIVE
         ELSE
            SET LIBRARY TO &lcTemp
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         lcTemp = This.uOldSet
         SET LIBRARY TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetLock AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "LOCK", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET LOCK ON
         OTHERWISE
            SET LOCK OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET LOCK ON
         OTHERWISE
            SET LOCK OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetLogErrors AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "LOGERRORS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET LOGERRORS ON
         OTHERWISE
            SET LOGERRORS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET LOGERRORS ON
         OTHERWISE
            SET LOGERRORS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMargin AS Set
*------------------------------------------------------------------------------
   uDefault = 0

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      * VFP sets a maximum of 256 when given a higher number.
      IF DODEFAULT( "MARGIN", MIN( 256, NVL( tnValue, This.uDefault )) )
         SET MARGIN TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET MARGIN TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMackey AS Set
*------------------------------------------------------------------------------
   uDefault = "SHIFT+F10"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "MACKEY", tcValue )
         lcTemp = This.uNewSet
         SET MACKEY TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         lcTemp = This.uOldSet
         SET MACKEY TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMark AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "MARK", tcValue )
         SET MARK TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET MARK TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMemoWidth AS Set
*------------------------------------------------------------------------------
   uDefault = 50

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      * VFP sets a maximum of 8192 when given a higher number.
      IF DODEFAULT( "MEMOWIDTH", MIN( 8192, NVL( tnValue, This.uDefault )) )
         SET MEMOWIDTH TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET MEMOWIDTH TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMessage AS SetTwo
*------------------------------------------------------------------------------
   uDefaultTwo = ""  && using #2 for SET( ... , 1 ) to save

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "MESSAGE", , tcValue, 2 )
         IF EMPTY( This.uNewSetTwo )
            SET MESSAGE TO
         ELSE
            SET MESSAGE TO This.uNewSetTwo
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         IF EMPTY( This.uOldSetTwo )
            SET MESSAGE TO
         ELSE
            SET MESSAGE TO This.uOldSetTwo
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetMultiLocks AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "MULTILOCKS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET MULTILOCKS ON
         OTHERWISE
            SET MULTILOCKS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET MULTILOCKS ON
         OTHERWISE
            SET MULTILOCKS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetNear AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "NEAR", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET NEAR ON
         OTHERWISE
            SET NEAR OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET NEAR ON
         OTHERWISE
            SET NEAR OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetNotify AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "NOTIFY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET NOTIFY ON
         OTHERWISE
            SET NOTIFY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET NOTIFY ON
         OTHERWISE
            SET NOTIFY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetNull AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "NULL", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET NULL ON
         OTHERWISE
            SET NULL OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET NULL ON
         OTHERWISE
            SET NULL OFF
      ENDCASE
ENDDEFINE

*------------------------------------------------------------------------------
DEFINE CLASS SetNullDisplay AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "NULLDISPLAY", tcValue )
         SET NULLDISPLAY TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET NULLDISPLAY TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetOdometer AS Set
*------------------------------------------------------------------------------
   uDefault = 100

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "ODOMETER", tnValue )
         SET ODOMETER TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET ODOMETER TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetOLEObject AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "OLEOBJECT", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET OLEOBJECT ON
         OTHERWISE
            SET OLEOBJECT OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET OLEOBJECT ON
         OTHERWISE
            SET OLEOBJECT OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetOptimize AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "OPTIMIZE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET OPTIMIZE ON
         OTHERWISE
            SET OPTIMIZE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET OPTIMIZE ON
         OTHERWISE
            SET OPTIMIZE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetPalette AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "PALETTE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET PALETTE ON
         OTHERWISE
            SET PALETTE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET PALETTE ON
         OTHERWISE
            SET PALETTE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetPath AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF TYPE( "tcValue" ) = "L"
        tcValue= SET( "Path" )
      ENDIF
      IF DODEFAULT( "PATH", tcValue )
         IF ( NOT EMPTY( tcOption )) AND UPPER( ALLTRIM( tcOption )) == "ADDITIVE"
            SET PATH TO ( This.uNewSet ) ADDITIVE
         ELSE
            SET PATH TO ( This.uNewSet )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET PATH TO ( This.uOldSet )
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetPrinter AS SetOnOffTwo
*------------------------------------------------------------------------------
   * Limit: No way to get SET PRINTER FONT|STYLE settings
   * or COM port settings.
   uDefault    = "OFF"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init( tcOnOff, tcTo, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "PRINTER", ;
                                    tcOnOff, tcTo, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET PRINTER ON
         OTHERWISE
            SET PRINTER OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST( This.cSet, "2", "3" )
            * Do nothing.
         CASE EMPTY( This.uNewSetTwo ) OR This.uOldSetTwo == "PRN"
            SET PRINTER TO
         CASE ( NOT EMPTY( tcOption )) AND;
              UPPER( ALLTRIM( tcOption )) == "ADDITIVE"
            SET PRINTER TO &tcTo ADDITIVE
         OTHERWISE  && macros used to enable setting COM port specs
            SET PRINTER TO &tcTo
      ENDCASE  && of secondary set

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET PRINTER ON
            OTHERWISE
               SET PRINTER OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE EMPTY( This.uOldSetTwo ) OR This.uOldSetTwo == "PRN"
               SET PRINTER TO
            OTHERWISE  && macro won't help here
               SET PRINTER TO ( This.uOldSetTwo )
         ENDCASE  && of secondary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetPoint AS Set
*------------------------------------------------------------------------------
   uDefault = "."

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "POINT", tcValue )
         lcTemp = This.uNewSet
         SET POINT TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         lcTemp = This.uOldSet
         SET POINT TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetProcedure AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tcOption, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "PROCEDURE", tcValue )
         IF EMPTY( This.uNewSet )
            SET PROCEDURE TO
         ELSE
            LOCAL lcTemp
            lcTemp = This.uNewSet
            IF ( NOT EMPTY( tcOption )) AND;
               ( UPPER( ALLTRIM( tcOption ))="ADDITIVE" )
               SET PROCEDURE TO &lcTemp ADDITIVE
            ELSE
               SET PROCEDURE TO &lcTemp
            ENDIF
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         LOCAL lcTemp
         lcTemp = This.uOldSet
         SET PROCEDURE TO &lcTemp
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetReadBorder AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "READBORDER", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET READBORDER ON
         OTHERWISE
            SET READBORDER OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET READBORDER ON
         OTHERWISE
            SET READBORDER OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetRefresh AS SetTwo
*------------------------------------------------------------------------------
   uDefault    = 0
   uDefaultTwo = 5

   PROTECTED PROCEDURE Init( tnEditSeconds, tnBufferSeconds, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      *-- Bounds for the first parameter is 0...3600
      IF VARTYPE( tnEditSeconds ) <> "N"
      	tnEditSeconds= This.uDefault
      ENDIF
      tnEditSeconds= MIN( MAX( tnEditSeconds, 0 ), 3600 )

			IF VARTYPE( tnBufferSeconds ) <> "N"
      	tnEditSeconds= this.uDefaultTwo
      ENDIF
      tnEditSeconds= MIN( MAX( tnEditSeconds, -1 ), 3600 )

      DO CASE
         CASE NOT DODEFAULT( "REFRESH", tnEditSeconds, ;
                               tnBufferSeconds, PARAMETERS() )
            * Do nothing.
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE ISNULL( tnEditSeconds ) AND ISNULL( tnBufferSeconds )
            * Do nothing.
         CASE tnEditSeconds < 0 OR tnBufferSeconds < 0
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE tnEditSeconds >= 0 AND tnBufferSeconds >= 0
            * Set both
            SET REFRESH TO tnEditSeconds, tnBufferSeconds
         CASE tnEditSeconds >= 0
            * Set first only
            SET REFRESH TO tnEditSeconds
         CASE tnBufferSeconds >= 0
            * Must set both to set the second
            lnTemp = SET( "REFRESH" )
            SET REFRESH TO lnTemp, tnBufferSeconds
         OTHERWISE
            ERROR "CASE...OTHERWISE: Unexpected."
      ENDCASE

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE
            CASE This.uOldSet >= 0 AND This.uOldSetTwo >= 0
               * Set both
               SET REFRESH TO This.uOldSet, This.uOldSetTwo
            CASE This.uOldSet >= 0
               * Set first only
               SET REFRESH TO This.uOldSet
            OTHERWISE
               ERROR "CASE...OTHERWISE: Unexpected."
         ENDCASE
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetReprocess AS Set
*------------------------------------------------------------------------------
* If the old set is to <n> SECONDS, it will be reset as just <n>
* because DISPLAY STATUS is the only way in VFP to detect when set
* to SECONDS.

   uDefault = 0

   PROTECTED cType

   PROTECTED PROCEDURE Init( tuValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      This.cType = TYPE( "tuValue" )
      DO CASE
         CASE ISNULL( tuValue )
            =DODEFAULT( "REPROCESS", tuValue )
         CASE ( NOT This.cType $ "CN" ) OR;
              ( NOT DODEFAULT( "REPROCESS", ;
                             IIF( This.cType=="C", ;
                                 UPPER( ALLTRIM( tuValue )), ;
                                 tuValue )) )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE This.cType == "C"
            DO CASE
               CASE This.uNewSet == "AUTOMATIC"
                  SET REPROCESS TO AUTOMATIC
               CASE RIGHT( This.uNewSet, 7 ) == "SECONDS"
                  SET REPROCESS TO VAL( This.uNewSet ) SECONDS
               OTHERWISE
                  ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
                  RETURN .F.  && early exit
            ENDCASE
         OTHERWISE
            SET REPROCESS TO This.uNewSet
      ENDCASE

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET REPROCESS TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetResource AS SetOnOffTwo
*------------------------------------------------------------------------------
   uDefault    = "ON"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init( tcOnOff, tcTo, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      * SET RESOURCE TO is first because it also sets it ON.
      DO CASE  && of secondary set
         CASE NOT DODEFAULT( "RESOURCE", ;
                                    tcOnOff, tcTo, ;
                                    PARAMETERS() )
            RETURN .F.  && early exit
         CASE NOT INLIST( This.cSet, "2", "3" )
            * Do nothing.
         CASE EMPTY( This.uNewSetTwo )
            SET RESOURCE TO
         OTHERWISE
            SET RESOURCE TO ( This.uNewSetTwo )
      ENDCASE  && of secondary set

      DO CASE  && of primary set
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         CASE This.uNewSet == "ON"
            SET RESOURCE ON
         OTHERWISE
            SET RESOURCE OFF
      ENDCASE  && of primary set

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         * SET RESOURCE TO is first because it also sets it ON.
         DO CASE  && of secondary set
            CASE NOT INLIST( This.cSet, "2", "3" )
               * Do nothing.
            CASE EMPTY( This.uOldSetTwo )
               SET RESOURCE TO
            OTHERWISE
               SET RESOURCE TO ( This.uOldSetTwo )
         ENDCASE  && of secondary set

         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            CASE This.uOldSet == "ON"
               SET RESOURCE ON
            OTHERWISE
               SET RESOURCE OFF
         ENDCASE  && of primary set
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetResourceCreate AS SetResource
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcOnOff, tcTo, tlNoReset )
      LOCAL lcTo
      lcTo = IIF( EMPTY( tcTo ), ;
                 HOME() + "FoxUser.dbf", ;
                 TRIM( tcTo ) + IIF( "." $ tcTo, "", ".dbf" ))
      IF NOT ( FILE( lcTo ) OR This.CreateResource( lcTo ))
         RETURN .F.
      ELSE
         RETURN DODEFAULT( tcOnOff, tcTo, tlNoReset )
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF ( NOT This.lNoReset ) AND INLIST( This.cSet, "2", "3" )
         LOCAL lcTo
         lcTo = IIF( EMPTY( This.uOldSetTwo ), ;
                    HOME() + "FoxUser.dbf", ;
                    TRIM( This.uOldSetTwo ) + IIF( "." $ This.uOldSetTwo, ;
                                                "", ".dbf" ))
         IF NOT ( FILE( lcTo ) OR This.CreateResource( lcTo ))
            RETURN
         ELSE
            DODEFAULT()
         ENDIF
      ENDIF

   PROTECTED FUNCTION CreateResource( tcTable )
      LOCAL llReturn, ;
            loSaveSelect
      loSaveSelect = CREATEOBJECT( "SaveSelect" )
      CREATE TABLE ( tcTable ) FREE;
         ( Type     C( 12 ), ;
          Id       C( 12 ), ;
          Name     C( 24 ), ;
          ReadOnly L, ;
          CkVal    N( 6 ), ;
          Data     M, ;
          Updated  D )
      llReturn = UPPER( tcTable ) $ FULLPATH( DBF() )
      USE
      RETURN llReturn
   ENDFUNC   && CreateResource
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSafety AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "SAFETY", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET SAFETY ON
         OTHERWISE
            SET SAFETY OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET SAFETY ON
         OTHERWISE
            SET SAFETY OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSeconds AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "SECONDS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET SECONDS ON
         OTHERWISE
            SET SECONDS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET SECONDS ON
         OTHERWISE
            SET SECONDS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSeparator AS Set
*------------------------------------------------------------------------------
   uDefault = ", "   && "" will not work!

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "SEPARATOR", tcValue )
         SET SEPARATOR TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET SEPARATOR TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSpace AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "SPACE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET SPACE ON
         OTHERWISE
            SET SPACE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET SPACE ON
         OTHERWISE
            SET SPACE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetStatus AS SetOnOff
*------------------------------------------------------------------------------
   * Limit:  no way to get SET STATUS TIMEOUT TO <n> value
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "STATUS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET STATUS ON
         OTHERWISE
            SET STATUS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET STATUS ON
         OTHERWISE
            SET STATUS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetStatusBar AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "STATUS BAR", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET STATUS BAR ON
         OTHERWISE
            SET STATUS BAR OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET STATUS BAR ON
         OTHERWISE
            SET STATUS BAR OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetStep AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "STEP", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET STEP ON
         OTHERWISE
            SET STEP OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET STEP ON
         OTHERWISE
            SET STEP OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSysFormats AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "SYSFORMATS", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET SYSFORMATS ON
         OTHERWISE
            SET SYSFORMATS OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET SYSFORMATS ON
         OTHERWISE
            SET SYSFORMATS OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSysMenu AS Set
*------------------------------------------------------------------------------
* Handles only ON, OFF, and AUTOMATIC.  Does not handle SET TO.

   uDefault = "AUTOMATIC"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE ISNULL( tcValue )
            =DODEFAULT( "SYSMENU", tcValue )
         CASE NOT INLIST( UPPER( ALLTRIM( tcValue )), ;
                         "ON", "OFF", "AUTOMATIC" )
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            LOCAL lcValue
            lcValue = UPPER( ALLTRIM( tcValue ))
            =DODEFAULT( "SYSMENU", lcValue )
            DO CASE
               CASE lcValue == "AUTOMATIC"
                  SET SYSMENU AUTOMATIC
               CASE lcValue == "ON"
                  SET SYSMENU ON
               CASE lcValue == "OFF"
                  SET SYSMENU OFF
            ENDCASE
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "AUTOMATIC"
            SET SYSMENU AUTOMATIC
         CASE This.uOldSet == "ON"
            SET SYSMENU ON
         CASE This.uOldSet == "OFF"
            SET SYSMENU OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTableValidate AS Set
*------------------------------------------------------------------------------
   uDefault = 2

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "TableValidate", tnValue )
         SET TableValidate TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET TableValidate TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTalk AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "TALK", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET TALK ON
         OTHERWISE
            SET TALK OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET TALK ON
         OTHERWISE
            SET TALK OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTopic AS SetTwo
*------------------------------------------------------------------------------
   uDefault    = ""
   uDefaultTwo = 0

   PROTECTED PROCEDURE Init( tcTopic, tcID, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT( "TOPIC", ;
                               tcTopic, ;
                               tcID, ;
                               PARAMETERS() )
         CASE NOT INLIST( This.cSet, "1", "3" )
            * Do nothing.
         OTHERWISE
            LOCAL lcTopic
            lcTopic = This.uNewSet
            SET TOPIC TO &lcTopic
      ENDCASE  && of primary set

      * Secondary set.
      IF INLIST( This.cSet, "2", "3" )
         SET TOPIC ID TO This.uNewSetTwo
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST( This.cSet, "1", "3" )
               * Do nothing.
            OTHERWISE
               LOCAL lcTopic
               lcTopic = This.uOldSet
               SET TOPIC TO &lcTopic
         ENDCASE  && of primary set

         * Secondary set.
         IF INLIST( This.cSet, "2", "3" )
            SET TOPIC ID TO This.uOldSetTwo
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTrBetween AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "TRBETWEEN", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET TRBETWEEN ON
         OTHERWISE
            SET TRBETWEEN OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET TRBETWEEN ON
         OTHERWISE
            SET TRBETWEEN OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetTypeahead AS Set
*------------------------------------------------------------------------------
   uDefault = 20

   PROTECTED PROCEDURE Init( tnValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "TYPEAHEAD", tnValue )
         SET TYPEAHEAD TO This.uNewSet
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         SET TYPEAHEAD TO This.uOldSet
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetUdfParms AS Set
*------------------------------------------------------------------------------
   uDefault = "VALUE"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "UDFPARMS", ;
                            IIF( ISNULL( tcValue ), ;
                                tcValue, UPPER( ALLTRIM( tcValue )) ))
            RETURN .F.  && early exit
         CASE This.uNewSet == "VALUE"
            SET UDFPARMS VALUE
         CASE This.uNewSet == "REFERENCE"
            SET UDFPARMS REFERENCE
         OTHERWISE
            ERROR 231  &&  was: cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "VALUE"
            SET UDFPARMS VALUE
         OTHERWISE
            SET UDFPARMS REFERENCE
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetUnique AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "UNIQUE", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET UNIQUE ON
         OTHERWISE
            SET UNIQUE OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET UNIQUE ON
         OTHERWISE
            SET UNIQUE OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetVarcharMapping AS SetOnOff
*------------------------------------------------------------------------------
   uDefault = "ON"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "VarcharMapping", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET VarcharMapping ON
         OTHERWISE
            SET VarcharMapping OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET VarcharMapping ON
         OTHERWISE
            SET VarcharMapping OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetView AS SetOnOff
*------------------------------------------------------------------------------
* Does not handle SET VIEW TO.
   uDefault = "OFF"

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT( "VIEW", tcValue )
            RETURN .F.  && early exit
         CASE This.uNewSet == "ON"
            SET VIEW ON
         OTHERWISE
            SET VIEW OFF
      ENDCASE

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE This.uOldSet == "ON"
            SET VIEW ON
         OTHERWISE
            SET VIEW OFF
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetWindowOfMemo AS Set
*------------------------------------------------------------------------------
   uDefault = ""

   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF DODEFAULT( "WINDOW", tcValue )
         lcTemp = This.uNewSet
         SET WINDOW OF MEMO TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF NOT This.lNoReset
         lcTemp = This.uOldSet
         SET WINDOW OF MEMO TO &lcTemp
      ENDIF
ENDDEFINE


*************************************************************
* SET Default Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SetVfpDefaults AS Custom
*------------------------------------------------------------------------------
* Visual FoxPro Defaults.
   PROTECTED PROCEDURE Init( tlNoReset )
      This.AddObject( "SetAlternate", "SetAlternate", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetAnsi", "SetAnsi", .NULL., tlNoReset )
      This.AddObject( "SetAutosave", "SetAutosave", .NULL., tlNoReset )
      This.AddObject( "SetBell", "SetBell", .NULL., tlNoReset )
      This.AddObject( "SetBlocksize", "SetBlocksize", .NULL., tlNoReset )
      This.AddObject( "SetBrstatus", "SetBrstatus", .NULL., tlNoReset )
      This.AddObject( "SetCarry", "SetCarry", .NULL., tlNoReset )
      This.AddObject( "SetCentury", "SetCentury", .NULL., tlNoReset )
      This.AddObject( "SetClear", "SetClear", .NULL., tlNoReset )
      This.AddObject( "SetClock", "SetClock", .NULL., tlNoReset )
      This.AddObject( "SetCollate", "SetCollate", .NULL., tlNoReset )
      This.AddObject( "SetColor", "SetColor", .NULL., tlNoReset )
      This.AddObject( "SetCompatible", "SetCompatible", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetConfirm", "SetConfirm", .NULL., tlNoReset )
      This.AddObject( "SetConsole", "SetConsole", .NULL., tlNoReset )
      This.AddObject( "SetCpcompile", "SetCpcompile", .NULL., tlNoReset )
      This.AddObject( "SetCpdialog", "SetCpdialog", .NULL., tlNoReset )
      This.AddObject( "SetCurrency", "SetCurrency", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetCursor", "SetCursor", .NULL., tlNoReset )
      This.AddObject( "SetDatabase", "SetDatabase", .NULL., tlNoReset )
      This.AddObject( "SetDataSession", "SetDataSession", .NULL., tlNoReset )
      This.AddObject( "SetDate", "SetDate", .NULL., tlNoReset )
      This.AddObject( "SetDebug", "SetDebug", .NULL., tlNoReset )
      This.AddObject( "SetDecimals", "SetDecimals", .NULL., tlNoReset )
      This.AddObject( "SetDefault", "SetDefault", .NULL., tlNoReset )
      This.AddObject( "SetDeleted", "SetDeleted", .NULL., tlNoReset )
      This.AddObject( "SetDelimiters", "SetDelimiters", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetDevelopment", "SetDevelopment", .NULL., tlNoReset )
      This.AddObject( "SetDisplay", "SetDisplay", .NULL., tlNoReset )
      This.AddObject( "SetDohistory", "SetDohistory", .NULL., tlNoReset )
      This.AddObject( "SetEcho", "SetEcho", .NULL., tlNoReset )
      This.AddObject( "SetEscape", "SetEscape", .NULL., tlNoReset )
      This.AddObject( "SetExact", "SetExact", .NULL., tlNoReset )
      This.AddObject( "SetExclusive", "SetExclusive", .NULL., tlNoReset )
      This.AddObject( "SetFdow", "SetFdow", .NULL., tlNoReset )
      This.AddObject( "SetFixed", "SetFixed", .NULL., tlNoReset )
      This.AddObject( "SetFullPath", "SetFullPath", .NULL., tlNoReset )
      This.AddObject( "SetFweek", "SetFweek", .NULL., tlNoReset )
      This.AddObject( "SetHeadings", "SetHeadings", .NULL., tlNoReset )
      This.AddObject( "SetHelp", "SetHelp", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetHelpFilter", "SetHelpFilter", .NULL., tlNoReset )
      This.AddObject( "SetHours", "SetHours", .NULL., tlNoReset )
      This.AddObject( "SetIntensity", "SetIntensity", .NULL., tlNoReset )
      This.AddObject( "SetKeycomp", "SetKeycomp", .NULL., tlNoReset )
      This.AddObject( "SetLibrary", "SetLibrary", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetLock", "SetLock", .NULL., tlNoReset )
      This.AddObject( "SetLogErrors", "SetLogErrors", .NULL., tlNoReset )
      This.AddObject( "SetMargin", "SetMargin", .NULL., tlNoReset )
      This.AddObject( "SetMackey", "SetMackey", .NULL., tlNoReset )
      This.AddObject( "SetMark", "SetMark", .NULL., tlNoReset )
      This.AddObject( "SetMemoWidth", "SetMemoWidth", .NULL., tlNoReset )
      This.AddObject( "SetMultiLocks", "SetMultiLocks", .NULL., tlNoReset )
      This.AddObject( "SetNear", "SetNear", .NULL., tlNoReset )
      This.AddObject( "SetNotify", "SetNotify", .NULL., tlNoReset )
      This.AddObject( "SetNull", "SetNull", .NULL., tlNoReset )
      This.AddObject( "SetOdometer", "SetOdometer", .NULL., tlNoReset )
      This.AddObject( "SetOLEObject", "SetOLEObject", .NULL., tlNoReset )
      This.AddObject( "SetOptimize", "SetOptimize", .NULL., tlNoReset )
      This.AddObject( "SetPalette", "SetPalette", .NULL., tlNoReset )
      This.AddObject( "SetPath", "SetPath", .NULL., tlNoReset )
      This.AddObject( "SetPrinter", "SetPrinter", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetPoint", "SetPoint", .NULL., tlNoReset )
      This.AddObject( "SetProcedure", "SetProcedure", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetReadBorder", "SetReadBorder", .NULL., tlNoReset )
      This.AddObject( "SetRefresh", "SetRefresh", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetReprocess", "SetReprocess", .NULL., tlNoReset )
      This.AddObject( "SetResource", "SetResource", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetSafety", "SetSafety", .NULL., tlNoReset )
      This.AddObject( "SetSeconds", "SetSeconds", .NULL., tlNoReset )
      This.AddObject( "SetSeparator", "SetSeparator", .NULL., tlNoReset )
      This.AddObject( "SetSpace", "SetSpace", .NULL., tlNoReset )
      This.AddObject( "SetStatus", "SetStatus", .NULL., tlNoReset )
      This.AddObject( "SetStatusBar", "SetStatusBar", .NULL., tlNoReset )
      This.AddObject( "SetStep", "SetStep", .NULL., tlNoReset )
      This.AddObject( "SetSysFormats", "SetSysFormats", .NULL., tlNoReset )
      This.AddObject( "SetSysMenu", "SetSysMenu", .NULL., tlNoReset )
      This.AddObject( "SetTalk", "SetTalk", .NULL., tlNoReset )
      This.AddObject( "SetTrBetween", "SetTrBetween", .NULL., tlNoReset )
      This.AddObject( "SetTopic", "SetTopic", .NULL., .NULL., tlNoReset )
      This.AddObject( "SetTypeAhead", "SetTypeAhead", .NULL., tlNoReset )
      This.AddObject( "SetUdfParms", "SetUdfParms", .NULL., tlNoReset )
      This.AddObject( "SetUnique", "SetUnique", .NULL., tlNoReset )
      This.AddObject( "SetView", "SetView", .NULL., tlNoReset )
      This.AddObject( "SetWindowOfMemo", "SetWindowOfMemo", ;
                      .NULL., tlNoReset )
      * SetClassLib must be last if this is a VCX.  Could be smarter
      * and keep itself in memory or ignore this if we're a VCX.
      This.AddObject( "SetClassLib", "SetClassLib", .NULL., .NULL., tlNoReset )
      RETURN .F.
ENDDEFINE


*************************************************************
* ON Parent Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS On AS Custom  && abstract class
*------------------------------------------------------------------------------
   PROTECTED cOldOn, ;
             cNewOn, ;
             lNoReset

   FUNCTION GetOld
      RETURN This.cOldOn

   FUNCTION GetNew
      RETURN This.cNewOn

   PROTECTED PROCEDURE Init( tcOn, tcValue )
      This.cOldOn = ON( tcOn )
      This.cNewOn = NVL( tcValue, "" )
ENDDEFINE


*************************************************************
* ON Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS OnError AS On
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      =DODEFAULT( "ERROR", tcValue )
      IF EMPTY( This.cNewOn )
         ON ERROR
      ELSE
         LOCAL lcError
         lcError = This.cNewOn
         ON ERROR &lcError
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE EMPTY( This.cOldOn )
            ON ERROR
         OTHERWISE
            LOCAL lcError
            lcError = This.cOldOn
            ON ERROR &lcError
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS OnKey AS On
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      =DODEFAULT( "KEY", tcValue )
      IF EMPTY( This.cNewOn )
         ON KEY
      ELSE
         LOCAL lcKey
         lcKey = This.cNewOn
         ON KEY &lcKey
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE EMPTY( This.cOldOn )
            ON KEY
         OTHERWISE
            LOCAL lcKey
            lcKey = This.cOldOn
            ON KEY &lcKey
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS OnKeyLabel AS On
*------------------------------------------------------------------------------
   PROTECTED cLabel

   FUNCTION GetLabel
      RETURN This.cLabel

   PROTECTED PROCEDURE Init( tcLabel, tcValue, tlNoReset )
   * Override parent class.
      This.cLabel = tcLabel
      This.cOldOn = ON( "KEY", tcLabel )
      This.cNewOn = NVL( tcValue, "" )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      IF EMPTY( This.cNewOn )
         ON KEY LABEL ( This.cLabel )
      ELSE
         LOCAL lcKey
         lcKey = This.cNewOn
         ON KEY LABEL ( This.cLabel ) &lcKey
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE EMPTY( This.cOldOn )
            ON KEY LABEL ( This.cLabel )
         OTHERWISE
            LOCAL lcKey
            lcKey = This.cOldOn
            ON KEY LABEL ( This.cLabel ) &lcKey
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS OnShutDown AS On
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcValue, tlNoReset )
      IF tlNoReset
         This.lNoReset = .T.
      ENDIF
      =DODEFAULT( "SHUTDOWN", tcValue )
      IF EMPTY( This.cNewOn )
         ON SHUTDOWN
      ELSE
         LOCAL lcShutDown
         lcShutDown = This.cNewOn
         ON SHUTDOWN &lcShutDown
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE This.lNoReset
            * Do nothing.
         CASE EMPTY( This.cOldOn )
            ON SHUTDOWN
         OTHERWISE
            LOCAL lcShutDown
            lcShutDown = This.cOldOn
            ON SHUTDOWN &lcShutDown
      ENDCASE
ENDDEFINE


*************************************************************
* Save/Restore Table Parent Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SaveArea AS Custom  && abstract class
*------------------------------------------------------------------------------
   PROTECTED nSelect

   FUNCTION GetSelect
      RETURN This.nSelect

   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      DO CASE
         CASE EMPTY( tuArea ) OR ISNULL( tuArea )
            This.nSelect = SELECT( 0 )
         CASE TYPE( "tuArea" ) == "N"
            This.nSelect = MAX( 0, tuArea )
         OTHERWISE  && assumes character or error will prevent init
            This.nSelect = SELECT( tuArea )
      ENDCASE
      IF EMPTY( This.nSelect )
         ERROR 17  &&  was: cnVF_ERR_TABLE_NUMINVALID
         RETURN .F.
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveUsedArea AS SaveArea  && abstract class
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      DO CASE
         CASE NOT DODEFAULT( tuArea )
            RETURN .F.  && early exit
         CASE NOT USED( This.nSelect )
            ERROR 52  &&  was: cnVF_ERR_TABLE_NOTOPEN
            RETURN .F.  && early exit
      ENDCASE

   PROTECTED PROCEDURE Destroy
      RETURN USED( This.nSelect )
ENDDEFINE


*************************************************************
* Set/Restore Table Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SaveSelect AS SaveArea
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      RETURN DODEFAULT( tuArea )

   PROTECTED PROCEDURE Destroy
      SELECT ( This.nSelect )
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetSelect AS SaveSelect
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tuNewArea )  && character or numeric
      IF DODEFAULT()  && current area
         SELECT ( tuNewArea )
      ELSE
         RETURN .F.
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveBuffering AS SaveUsedArea
*------------------------------------------------------------------------------
   PROTECTED nBuffering

   FUNCTION GetOld
      RETURN This.nBuffering

   PROTECTED PROCEDURE Init( tuArea )
      IF DODEFAULT( tuArea )
         This.nBuffering = CURSORGETPROP( "Buffering", This.nSelect )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         * This test prevents crashing when we're in a Transaction and no buffering change has been made.
      	 IF CURSORGETPROP( "Buffering", This.nSelect ) <> This.nBuffering
            CURSORSETPROP( "Buffering", This.nBuffering, This.nSelect )
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetBuffering AS SaveBuffering
*------------------------------------------------------------------------------
   PROTECTED nDefault
   nDefault = 1

   PROTECTED PROCEDURE GetDefault
      RETURN This.nDefault

   PROTECTED PROCEDURE Init( tnBuffering, tuNewArea )
      IF DODEFAULT( tuNewArea )
         =CURSORSETPROP( "Buffering", ;
                        NVL( tnBuffering, This.nDefault ), ;
                        This.nSelect )
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveRecno AS SaveUsedArea
*------------------------------------------------------------------------------
   PROTECTED nRecno

   FUNCTION GetOld
      RETURN This.nRecno

   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      IF DODEFAULT( tuArea )
         This.nRecno = IIF( EOF( This.nSelect ), ;
                            .NULL., RECNO( This.nSelect ))
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE NOT DODEFAULT()
            * Do nothing.
         CASE ISNULL( This.nRecno )  && EOF()
            This.AddObject( "SetSelect", "SetSelect", This.nSelect )
            LOCATE FOR .F.  && EOF()
         CASE This.nRecno <= RECCOUNT( This.nSelect )
            GO This.nRecno IN ( This.nSelect )
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveOrder AS SaveUsedArea
*------------------------------------------------------------------------------
* Only handles CDX tags, not individual IDX.
   PROTECTED cOrder, lDescending

   FUNCTION GetOld
      RETURN This.cOrder

   FUNCTION GetDescending
      RETURN This.lDescending

   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      IF DODEFAULT( tuArea )
         LOCAL lnSelect
         lnSelect    = This.nSelect
         This.cOrder = ORDER( lnSelect )
         IF NOT EMPTY( This.cOrder )
            This.lDescending = DESCENDING( TAGNO( ORDER( lnSelect ), ;
                                                 CDX( 1, lnSelect ), ;
                                                 lnSelect ), ;
                                           lnSelect )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE NOT DODEFAULT()
            * Do nothing.
         CASE EMPTY( This.cOrder )
            SET ORDER TO 0 IN ( This.nSelect )
         CASE This.lDescending
            SET ORDER TO ( This.cOrder ) IN ( This.nSelect );
                         DESCENDING
         OTHERWISE
            SET ORDER TO ( This.cOrder ) IN ( This.nSelect );
                         ASCENDING
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetOrder AS SaveOrder
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tuOrder, tuNewArea, tlDescending )
      DO CASE
         CASE NOT DODEFAULT( tuNewArea )
            RETURN .F.  && early exit
         CASE EMPTY( tuOrder ) OR ISNULL( tuOrder )
            SET ORDER TO 0 IN ( This.nSelect )
         CASE tlDescending
            SET ORDER TO ( tuOrder ) IN ( This.nSelect );
                         DESCENDING
         OTHERWISE
            SET ORDER TO ( tuOrder ) IN ( This.nSelect )
      ENDCASE
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveFilter AS SaveUsedArea
*------------------------------------------------------------------------------
   PROTECTED cFilter

   FUNCTION GetOld
      RETURN This.cFilter

   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      IF DODEFAULT( tuArea )
         This.cFilter = FILTER( This.nSelect )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         This.AddObject( "SetSelect", "SetSelect", This.nSelect )
         IF EMPTY( This.cFilter )
            SET FILTER TO
         ELSE
            LOCAL lcFilter
            lcFilter = This.cFilter
            SET FILTER TO &lcFilter
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetFilter AS SaveFilter
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcFilter, tuNewArea, tcAdditive )
   * tcAdditive ::= "AND" | "OR"
      IF DODEFAULT( tuNewArea )
         LOCAL loSelect
         loSelect = CREATEOBJECT( "SetSelect", This.nSelect )
         DO CASE
            CASE EMPTY( tcFilter ) OR ISNULL( tcFilter )
               SET FILTER TO
            CASE EMPTY( tcAdditive )
               SET FILTER TO &tcFilter
            OTHERWISE
               LOCAL lcFilter
               lcFilter = "( " + FILTER() + " ) " + tcAdditive+;
                          " ( " + tcFilter + " )"
               SET FILTER TO &lcFilter
         ENDCASE
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveRelation AS SaveUsedArea
*------------------------------------------------------------------------------
* Also handles SET SKIP.
   PROTECTED cRelation, ;
             cSkip

   FUNCTION GetOld
      RETURN This.cRelation

   PROTECTED PROCEDURE Init( tuArea )  && character or numeric
      IF DODEFAULT( tuArea )
         LOCAL loSelect
         loSelect = CREATEOBJECT( "SetSelect", This.nSelect )
         This.cRelation = SET( "RELATION" )
         This.cSkip     = SET( "SKIP" )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         LOCAL loSelect
         loSelect = CREATEOBJECT( "SetSelect", This.nSelect )
         IF EMPTY( This.cRelation )
            SET RELATION TO
         ELSE
            LOCAL lcTemp
            lcTemp = This.cRelation
            SET RELATION TO &lcTemp
            IF NOT EMPTY( This.cSkip )
               lcTemp = This.cSkip
               SET SKIP TO &lcTemp
            ENDIF
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetRelation AS SaveRelation
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( tcRelation, tuNewArea, tcSkip )
      IF DODEFAULT( tuNewArea )
         LOCAL loSelect
         loSelect = CREATEOBJECT( "SetSelect", This.nSelect )
         IF EMPTY( tcRelation ) OR ISNULL( tcRelation )
            SET RELATION TO
         ELSE
            SET RELATION TO &tcRelation
            IF NOT EMPTY( tcSkip )
               SET SKIP TO &tcSkip
            ENDIF
         ENDIF
      ENDIF
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveTable AS SaveUsedArea
*------------------------------------------------------------------------------
   PROTECTED cAlias, ;
             cFile, ;
             cLock

   PROTECTED PROCEDURE Init( tuArea, tlNoDependencies )
      IF DODEFAULT( tuArea )
         LOCAL loFullPath, loSelect
         loSelect   = CREATEOBJECT( "SetSelect", This.nSelect )
         loFullPath = CREATEOBJECT( "SetFullPath", "ON" )
         This.AddObject( "SaveBuffering", "SaveBuffering" )
         This.AddObject( "SaveRecno", "SaveRecno" )
         This.AddObject( "SetDataSession", "SetDataSession" )
         IF NOT tlNoDependencies
            * Order and filter could have references to other tables.
            This.AddObject( "SaveOrder", "SaveOrder" )
            This.AddObject( "SaveFilter", "SaveFilter" )
            This.AddObject( "SaveRelation", "SaveRelation" )
         ENDIF
         This.cAlias = ALIAS()
         This.cFile  = DBF()
         This.cLock  = SYS( 2011 )
      ELSE
         RETURN .F.
      ENDIF

   PROTECTED PROCEDURE Destroy
   * Override parent class which checks for an open table.
      LOCAL loSelect
      loSelect = CREATEOBJECT( "SetSelect", This.nSelect )
      IF NOT ALIAS() == This.cAlias
         This.RemoveObject( "SetDataSession", "SetDataSession" )
         IF USED( This.cAlias )  && close if open in another area
            USE IN ( This.cAlias )
         ENDIF
         IF This.cLock == "Exclusive"
            USE ( This.cFile ) ALIAS ( This.cAlias ) AGAIN EXCLUSIVE
         ELSE
            USE ( This.cFile ) ALIAS ( This.cAlias ) AGAIN SHARED
            DO CASE
               CASE This.cLock == "File Locked"
                  =FLOCK()
               CASE This.cLock == "Record Locked"
                  This.RemoveObject( "SaveRecno", "SaveRecno" )
                  =RLOCK()
               OTHERWISE  && should never happen
                  ERROR "CASE...OTHERWISE: Unexpected."
            ENDCASE
         ENDIF
      ENDIF  && NOT ALIAS() == This.cAlias
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SaveAllTables AS Custom
*------------------------------------------------------------------------------
   ADD OBJECT PROTECTED SaveSelect AS SaveSelect

   PROTECTED PROCEDURE Init
      LOCAL lnCounter, laUsed[1]
      IF AUSED( laUsed ) > 0
         * AUSED sorts from most recently opened to least recently opened.
         * Destruction is reversed; first constructed are last destructed,
         * so save the dependencies before the tables so all tables are
         * open when any potential dependencies are restored.
         FOR lnCounter = 1 TO ALEN( laUsed, 1 )
            This.AddObject( "SaveRel" + LTRIM( STR( lnCounter )), ;
                            "SaveRelation", ;
                            laUsed[lnCounter, 2] )
            This.AddObject( "SaveFil" + LTRIM( STR( lnCounter )), ;
                            "SaveFilter", ;
                            laUsed[lnCounter, 2] )
         ENDFOR
         * Relations are dependent on order.
         FOR lnCounter = 1 TO ALEN( laUsed, 1 )
            This.AddObject( "SaveOrd" + LTRIM( STR( lnCounter )), ;
                            "SaveOrder", ;
                            laUsed[lnCounter, 2] )
         ENDFOR
         * All dependencies are dependent on tables.
         FOR lnCounter = 1 TO ALEN( laUsed, 1 )
            This.AddObject( "SaveTab" + LTRIM( STR( lnCounter )), ;
                            "SaveTable", ;
                            laUsed[lnCounter, 2], ;
                            .T. )  && tables will be restored first
         ENDFOR
      ELSE
         RETURN .F.
      ENDIF
ENDDEFINE

*------------------------------------------------------------------------------
DEFINE CLASS OpenAliasCheckpoint AS CUSTOM
* Quick and dirty class to close work areas that, upon destroy, were not
* open at object creation time.
*------------------------------------------------------------------------------
	DIMENSION aUsedAreas[1]
	aUsedAreas[1]=""

	*******************
	FUNCTION Init()
	*******************
	AUSED( This.aUsedAreas )  && Saving all the used workareas coming in.
	RETURN

	*******************
	FUNCTION Destroy()
	*******************
	*-- Cleaning up
	LOCAL laUsedNow[1], lnK
	AUSED( laUsedNow )
	FOR lnK= 1 TO ALEN( laUsedNow, 1 )
		IF  ASCAN( This.aUsedAreas, laUsedNow[ lnk, 1 ] ) = 0
			USE IN ( laUsedNow[ lnk, 1 ] )
		ENDIF
	ENDFOR
	RETURN

ENDDEFINE




*************************************************************
* Set/Restore Property Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SaveProperty AS Custom
* Use the Update method to save any changes.
* Use the Revert method or destroy the object to discard unsaved changes.
* Array properties: currently saves the first element only.
*------------------------------------------------------------------------------
   PROTECTED oObject, ;
             aProperties[1, 2], ;
             cProperty

   PROTECTED PROCEDURE Init( toObject, tcProperty )  && arguments are optional
      * Object default order: 1 = parameter, 2 = PARENT, ( 3 ) _SCREEN.
      * Saves all properties unless tcProperty passed.
      DO CASE
         CASE TYPE( "toObject" ) == "O"
            This.oObject = toObject
         CASE TYPE( "This.PARENT" ) == "O"
            This.oObject = This.PARENT
         OTHERWISE
            This.oObject = _SCREEN
      ENDCASE
      IF NOT EMPTY( tcProperty )
         This.cProperty = ALLTRIM( tcProperty )
      ENDIF
      This.Update()

   PROTECTED PROCEDURE Destroy
      This.Revert()

   PROCEDURE Update
      LOCAL lnCounter, ;
            laProperties[1]
      IF EMPTY( This.cProperty )
         DIMENSION This.aProperties[AMEMBERS( laProperties, This.oObject ), 2]
         FOR lnCounter = 1 TO ALEN( laProperties )
            IF TYPE( "This.oObject." + laProperties[lnCounter] ) $ "OU"
               This.aProperties[lnCounter, 1] = .NULL.
            ELSE
               This.aProperties[lnCounter, 1] = laProperties[lnCounter]
               This.aProperties[lnCounter, 2] =;
                  EVALUATE( "This.oObject." + laProperties[lnCounter] )
            ENDIF
         ENDFOR
      ELSE
         DIMENSION This.aProperties[1, 2]
         This.aProperties[1, 1] = This.cProperty
         This.aProperties[1, 2] = EVALUATE( "This.oObject." + This.cProperty )
      ENDIF

   PROCEDURE Revert
      LOCAL lnCounter
      FOR lnCounter = 1 TO ALEN( This.aProperties, 1 )
         IF ( NOT ISNULL( This.aProperties[lnCounter, 1] )) AND;
            ( TYPE( "This.aProperties[lnCounter, 2]" ) !=;
                TYPE( "This.oObject."+This.aProperties[lnCounter, 1] ) OR;
             This.aProperties[lnCounter, 2] !=;
                EVALUATE( "This.oObject."+;
                         This.aProperties[lnCounter, 1] ))
            STORE This.aProperties[lnCounter, 2];
               TO ( "This.oObject." + This.aProperties[lnCounter, 1] )
         ENDIF
      ENDFOR
ENDDEFINE


*------------------------------------------------------------------------------
DEFINE CLASS SetProperty AS SaveProperty
*------------------------------------------------------------------------------
   PROTECTED PROCEDURE Init( toObject, tcProperty, tuValue )
      DODEFAULT( toObject, tcProperty )
      STORE tuValue TO ( "This.oObject." + This.cProperty )
ENDDEFINE


*************************************************************
* Set/Restore System Variable Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SetSysVar AS Custom
*------------------------------------------------------------------------------
   PROTECTED cSysVar, ;
             uValue

   FUNCTION GetOld
      RETURN This.uValue

   PROTECTED PROCEDURE Init( tcSysVar, tuValue )
      This.cSysVar = tcSysVar
      This.uValue  = EVALUATE( tcSysVar )
      STORE tuValue TO ( tcSysVar )

   PROTECTED PROCEDURE Destroy
      STORE This.uValue TO ( This.cSysVar )
ENDDEFINE


*************************************************************
* Timer Classes
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS MessageTimer AS Timer
*------------------------------------------------------------------------------
* This class works differently from most in this library because
* it's not meant to be destroyed between message settings.  Instead,
* the timer method resets the message and the class stays available
* for another timed message.  This is similar to what a TIMEOUT clause
* on SET MESSAGE would do.
   PROTECTED Interval, ;
             nIntervalDefault, ;
             cMessage
   nIntervalDefault = 0
   cMessage = .NULL.

   PROCEDURE SetIntervalDefault( tnSeconds )
      This.nIntervalDefault = IIF( EMPTY( tnSeconds ), ;
                                  0, tnSeconds * 1000 )

   PROCEDURE SetMessage( tcMessage, tnSeconds )
      IF ISNULL( This.cMessage )  && don't get our timed message
         This.cMessage = SET( "MESSAGE", 1 )
      ENDIF
      This.Interval = IIF( PARAMETERS() < 2, ;
                          This.nIntervalDefault, ;
                          tnSeconds * 1000 )
      This.Reset()  && start over in case already in progress
      IF EMPTY( tcMessage )
         SET MESSAGE TO
      ELSE
         SET MESSAGE TO tcMessage
      ENDIF

   PROTECTED PROCEDURE Timer  && fires once to clear message
      IF EMPTY( This.cMessage )
         SET MESSAGE TO
      ELSE
         SET MESSAGE TO This.cMessage
      ENDIF
      This.cMessage = .NULL.
      This.Interval = 0  && don't fire until new message is set

   PROTECTED PROCEDURE Destroy
      IF NOT ISNULL( This.cMessage )
         This.Timer()
      ENDIF
ENDDEFINE


*************************************************************
* Lockscreen
*************************************************************

*------------------------------------------------------------------------------
DEFINE CLASS SetLockScreen AS CUSTOM
*-- Lockscreen management.  Doesn't completely fit with the envlib classes but
*-- a-propos nonetheless because it resets when it goes out of scope.
*------------------------------------------------------------------------------
  PROTECTED lOldLockScreen, loForm
  loForm= .F.
  lOldLockScreen= .F.

  FUNCTION Init( loCurrentForm, lNewSetting )
  This.loForm= loCurrentForm
  This.lOldLockScreen= loCurrentForm.LockScreen
  loCurrentForm.LockScreen= lNewSetting

  FUNCTION Destroy
  This.loForm.LockScreen= This.lOldLockScreen
ENDDEFINE


*** EnvLib.prg **********************************************

