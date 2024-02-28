//////////////////////////////////////////////////////////////////////
//
//  DBESYS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2003. All rights reserved.         
//  
//  Contents:
//      User-Defined DbeSys() routine loads DBFNTX and FOXCDX DBEs
//   
//////////////////////////////////////////////////////////////////////

/*
 * DbeSys() will be executed every time this program starts
 */
PROCEDURE dbeSys()

  /*
   *  Adaption of Sorting order to hosting environment
   */
   SET COLLATION TO SYSTEM


  /*
   *   The database engines DBFDBE and NTXDBE are loaded "hidden"
   *   and are combined to the abstract database engine DBFNTX
   */
   IF ! DbeLoad( "DBFDBE", .T.)
      Alert( "Database engine DBFDBE not loaded" , {"OK"} )
   ENDIF

   IF ! DbeLoad( "NTXDBE",.T.)
      Alert( "Database-Engine NTXDBE not loaded" , {"OK"} )
   ENDIF

   IF ! DbeBuild( "DBFNTX", "DBFDBE", "NTXDBE" )
      Alert( "DBFNTX Database-Engine;is not created" , {"OK"} )
   ENDIF


  /*
   *   The same is done with FOX and CDX DBEs
   */
   IF ! DbeLoad( "FOXDBE", .T.)
      Alert( "Database engine FOXDBE not loaded" , {"OK"} )
   ENDIF

   IF ! DbeLoad( "CDXDBE",.T.)
      Alert( "Database-Engine CDXDBE not loaded" , {"OK"} )
   ENDIF

   IF ! DbeBuild( "FOXCDX", "FOXDBE", "CDXDBE" )
      Alert( "FOXCDX Database-Engine;is not created" , {"OK"} )
   ENDIF


   DbeSetDefault( "DBFNTX" )

RETURN

//
// EOF
