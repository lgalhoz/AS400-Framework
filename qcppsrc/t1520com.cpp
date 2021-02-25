/*==================================================================*/
/* This program illustrates how to use commitment control using the */
/* _Rcommit() function and to rollback a transaction using the      */
/* _Rollbck() function. */
/*==================================================================*/
#include <stdio.h>
#include <recio.h>
#include <stdlib.h>
#include <string.h>
#define PF03 2
#define IND_OFF '0'
#define IND_ON '1'

int main(void) {
   char buf[40];
   int rc = 1;
   _SYSindara ind_area;
   _RFILE *purf;
   _RFILE *dailyf;
   _RFILE *monthlyf;

   //-----------------------------------------------------------------------
   // Open purchase display file, daily transaction file and monthly
   // transaction file.
   //-----------------------------------------------------------------------
   if ((purf = _Ropen ("*LIBL/T1520DD7", "ar+,indicators=y" )) == NULL) {
      printf ("Display file T1520DD7 did not open.\n" );
      exit (1 );
   }

   if ((dailyf = _Ropen ("*LIBL/T1520DD5", "wr,commit=n") ) == NULL ) {
      printf ("Daily transaction file T1520DD5 did not open.\n" );
      exit (2 );
   }

   if ((monthlyf = _Ropen ("*LIBL/T1520DD6","ar,commit=n") ) == NULL ) {
      printf ("Monthly transaction T1520DD6 file did not open.\n" );
      exit (3 );
   }

   //-----------------------------------------------------------------------
   // The associate separate indicator area with the purchase file. */
   //-----------------------------------------------------------------------
   _Rindara (purf, ind_area );

   //-----------------------------------------------------------------------
   // Select the purchase record format. */
   //-----------------------------------------------------------------------
   _Rformat (purf, "PURCHASE" );

   //-----------------------------------------------------------------------
   // Invite the user to enter a purchase transaction. */
   // The _Rwrite function writes the purchase display. */
   //-----------------------------------------------------------------------
   _Rwrite (purf, "", 0 );
   _Rreadn (purf, buf, sizeof(buf), __DFT );

   //-----------------------------------------------------------------------
   // While the user is entering transactions, update daily and */
   // monthly transaction files. */
   //-----------------------------------------------------------------------
   while (rc && ind_area[PF03] == IND_OFF ) {
      rc = ((_Rwrite (dailyf, buf, sizeof(buf) ))->num_bytes );
      rc = rc && (_Rwrite (monthlyf, buf, sizeof(buf) ))->num_bytes;

      //-----------------------------------------------------------------------
      // If the databases were updated, then commit transaction. */
      // Otherwise, rollback the transaction and indicate to the */
      // user that an error has occurred and end the application. */
      //-----------------------------------------------------------------------
      if (rc ) {
      // _Rcommit ("Transaction complete" );
      } else {
      // _Rrollbck ();
         _Rformat (purf, "ERROR" );
      }
      _Rwrite (purf, "", 0 );
      _Rreadn (purf, buf, sizeof(buf), __DFT );
   }
}
