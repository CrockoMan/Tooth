//////////////////////////////////////////////////////////////////////
//
//  DIRECTRY.CH
//
//  Copyright:
//      Alaska Software, (c) 1998-2006. All rights reserved.         
//  
//  Contents:
//      #define constants for the 2 dimensional array which is
//      returned by the function Directory().
//      -> File informations
//   
//////////////////////////////////////////////////////////////////////

// Directry.ch is not included
#ifndef  _DIRECTRY_CH        

// File name
#define  F_NAME           1  
// File size
#define  F_SIZE           2  
// Date of last write access
#define  F_WRITE_DATE     3  
// compatibility constant
#define  F_DATE           3  
// Time of last write access
#define  F_WRITE_TIME     4  
// compatibility constant
#define  F_TIME           4  
// File attribute
#define  F_ATTR           5  
// Size of extended attributes
#define  F_EA_SIZE        6  
// Creation date
#define  F_CREATION_DATE  7  
// Creation time
#define  F_CREATION_TIME  8  
// Date of last access
#define  F_ACCESS_DATE    9  
// Time of last access
#define  F_ACCESS_TIME      10  
                              
// Length of a sub-array which contains the file information
#define  F_LEN           10  

// Directry.ch is included
#define  _DIRECTRY_CH        

#endif        // #ifndef _DIRECTRY_CH

