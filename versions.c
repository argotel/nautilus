/*
 * The author of this software is J. Andrew Fingerhut.
 * Copyright (c) 1995 by J. Andrew Fingerhut.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* versions.c
 *
 * SCCS ID: @(#)versions.c 1.1 96/02/29 
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 95/08/24  J. A. Fingerhut    Initial version written.
 * 96/01/03  D. Miller		Fixed bug that would use name file 'upgrade'
 *				regardless of what UPGRADE_FILE was set to
 *				in configuration file
 */


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "nautilus.h"


/* external variables */
extern struct param_t params;   /* operating parameters */


/*
 * This is the maximum number of characters that a lower version of
 * Nautilus can receive in the upgrade information.  It should be
 * somewhat larger than we ever expect the string upgrade_info to
 * become.  An upper limit is made here only to prevent the receiver
 * from receiving data indefinitely.
 */
#define MAX_INFO_SIZE     16384

/*
 * The following string contains information on how to upgrade to the
 * most recently released version of Nautilus.  This information
 * should remain as stable as possible.
 */

static char *upgrade_info =
"For information about Nautilus, or to find out where you can\n\
download the latest version, see the Nautilus web page:\n\
\n\
\thttp://www.lila.com/nautilus/index.html\n";



/*

   FUNCTION: OpenUpgradeFile

   PURPOSE:

   Open the file with name UPGRADE_FILE for writing, unless that file
   already exists, or it cannot be opened for writing.

   ARGUMENTS:

   char *file_name

   The file name to open.

   RETURN VALUE:

   A pointer to the FILE structure if successful, or NULL if the file
   already exists, or cannot be opened for writing.
   
   SIDE EFFECTS:

   If NULL is returned, an error message is printed to stderr
   explaining the reason.
*/

FILE *
OpenUpgradeFile()
{
    FILE        *f;

    f = fopen(params.upgrade_file, "r");
    if (f == NULL) {
	/*
	 * Then the file does not exist already.  Attempt to open for writing.
	 */
	f = fopen(params.upgrade_file, "w");
	if (f == NULL) {
	    fprintf(stderr, "Upgrade file %s cannot be written.\n",
		    params.upgrade_file);
	    fflush(stderr);
	}
    }
    else {
	fprintf(stderr,
		"Upgrade file %s already exists.  It will not be overwritten.\n",
		params.upgrade_file);
	fflush(stderr);
	fclose(f);
	f = NULL;
    }

    return f;
}



/*

   FUNCTION: VersionsCompatible

   PURPOSE:

   Determine whether the current version is compatible with the older
   version specified in the arguments.

   ARGUMENTS:

   UINT8 old_major
   UINT8 old_minor

   The major and minor version number of the older version of Nautilus
   that wants to communicate with this one.

   RETURN VALUE:
   
   TRUE if the two versions are compatible, otherwise FALSE.

   SIDE EFFECTS: None
*/

int
VersionsCompatible(UINT8 old_major, UINT8 old_minor)
{
    /*
     * It is guaranteed that old_major >= 1, because this check is
     * made before VersionsCompatible is ever called.
     *
	 * As of version 1.7, full backward compatibility through
	 * version 1.0 is assured.  So we can just return TRUE here.
	 * In the future, it may be necessary to compare the arguments
	 * with VERSION_MAJOR and VERSION_MINOR in order to determine
	 * compatibility information.
	 */

    return TRUE;
}



/*

   FUNCTION: SendCompatible

   PURPOSE:

   This function is called by the newer of two versions of Nautilus
   participating in a call.  It sends a packet to the other program
   merely indicating one bit of information, TRUE or FALSE.

   ARGUMENTS:

   int compatible

   compatible is TRUE if and only if the newer version is compatible
   with the older one.

   RETURN VALUE: None
   
   SIDE EFFECTS: None
*/

void
SendCompatible(int compatible)
{
    UINT8 data;

    if (compatible) {
	data = (UINT8) 1;
    }
    else {
	data = (UINT8) 0;
    }
    SendPkt(RDATA, &data, 1, params.sp_timeout);
}



/*

   FUNCTION: ReceiveCompatible

   PURPOSE:

   This function is called by the older of two versions of Nautilus
   participating in a call.  It receives the one packet sent by
   SendCompatible.

   ARGUMENTS:

   int *compatible

   If a data packet with 1 byte is successfully received, and the 1
   byte is either 1 or 0, then the integer pointed to by this argument
   is set to TRUE or FALSE, respectively.  In all other cases, this
   value is not written.

   RETURN VALUE:

   SUCCESS if the integer pointed to by the argument is written,
   otherwise FAIL.
   
   SIDE EFFECTS: None
*/

int
ReceiveCompatible(int *compatible)
{
    struct packet_t    packet;

    if (ReadPkt(&packet, params.rp_timeout) != 2) {
/* This line changed due to bug in connecting to older versions */
/*    D Miller    06/26/97 */
/* if (ReadPkt(&packet, params.rp_timeout) != SUCCESS) { */
        return FAIL;
    }
    else {
        if ((packet.type != RDATA) || (packet.length != 1)) {
            return FAIL;
	}
	if (packet.data[0] == (UINT8) 1) {
	    *compatible = TRUE;
	    return SUCCESS;
	}
	else if (packet.data[0] == (UINT8) 0) {
	    *compatible = FALSE;
	    return SUCCESS;
	}
	else {
	    return FAIL;
	}
    }
}



/*

   FUNCTION: SendUpgradeInfo

   PURPOSE:

   This function is called by the newer of two versions of Nautilus
   participating in a call.  It sends a short file of unencrypted text
   that describes where to get the most recent version of Nautilus.
   It also displays a message on the screen of the participant with
   the newer version, so they also know why the call was terminated
   prematurely.

   ARGUMENTS:

   UINT8 old_major
   UINT8 old_minor

   The major and minor version numbers of the old version that the
   newer version is failing to communicate with.  This is only used
   in the display to the user of the newer version.

   RETURN VALUE:
   
   Currently always SUCCESS, but this could be changed so that it only
   returns SUCCESS if the receiver acknowledges receiving the message.
   The current version does not do this.

   SIDE EFFECTS: None
*/

int
SendUpgradeInfo(UINT8 old_major, UINT8 old_minor)
{
    char *local_notification_fmt =
"The other Nautilus user has version %d.%d, which is not compatible with\n\
your version, %s.\n\
\n\
Transmitting instructions on how to obtain the newest version...\n";
    char local_notification[512];

    int len, total_sent, send_this_time;


    sprintf(local_notification, local_notification_fmt,
	    old_major, old_minor, VERSION_STRING);

    error(MSG_ERROR, local_notification);

    /*
     * Send the contents of the string 'upgrade_info' to the older
     * version, where all packets except possibly the last one are
     * the maximum size allowed.  End with a REOT packet.
     */

    len = strlen(upgrade_info);

    total_sent = 0;
    while (total_sent < len) {
	send_this_time = len - total_sent;
	if (send_this_time > MAX_PKT_DATA) {
	    send_this_time = MAX_PKT_DATA;
	}
	SendPkt(RDATA, &upgrade_info[total_sent], send_this_time,
	        params.sp_timeout);
	total_sent += send_this_time;
    }
    SendPkt(REOT, NULL, 0, params.sp_timeout);

    error(MSG_WARNING, "Transmission complete.");

    return SUCCESS;
}



/*

   FUNCTION: ReceiveUpgradeInfo

   PURPOSE:

   This function is called by the older of two versions of Nautilus
   participating in a call.  It receives a short file of unencrypted
   text that describes where to get the most recent version of
   Nautilus.  This message is displayed on the screen, and it is saved
   in a file with name UPGRADE_FILE, assuming that file does not exist
   already.  We don't want to clobber an existing file with that name,
   since it might have nothing to do with Nautilus.

   Of course it would be even nicer if the program asked the user for
   a file name in which to save the information, and asked for
   confirmation if the specified file already exists, but I just want
   to get something basic working first.

   ARGUMENTS:

   UINT8 new_major
   UINT8 new_minor

   The major and minor version numbers of the new version that the
   older version is failing to communicate with.  This is only used
   in the display to the user of the older version.

   RETURN VALUE:

   SUCCESS if the file was completely received from the newer version,
   and displayed on the screen, even if it wasn't saved in a file.
   FAIL otherwise.
   
   SIDE EFFECTS: None
*/

int
ReceiveUpgradeInfo(UINT8 new_major, UINT8 new_minor)
{
    char           *local_notification_fmt =
"The other Nautilus user has version %d.%d, which is not compatible with\n\
your version, %s.\n\
\n\
Receiving instructions on how to obtain the newest version...\n\
\n\
\n";
    char           local_notification[512];
    struct packet_t packet;
    int            total_received, received_this_time;
    int            receive_overflow;
    FILE           *upgrade_f;
    int            i;


    sprintf(local_notification, local_notification_fmt,
	    new_major, new_minor, VERSION_STRING);

    error(MSG_ERROR, local_notification);

    /*
     * Check if the file UPGRADE_FILE already exists, and if not, if
     * it can be opened for writing.
     */
    upgrade_f = OpenUpgradeFile();

    /*
     * Continue reading DATA packets from the newer version,
     * displaying the received data on the screen, and saving the
     * received data into the file 'upgrade_f', if upgrade_f is not NULL.
     * Stop if any other type of packet is received, if more than
     * MAX_INFO_SIZE bytes are received, or if more than
     * params.rp_timeout seconds elapse between consecutive packet
     * arrivals.
     */
    receive_overflow = FALSE;
    total_received = 0;

    for (;;) {
	if (ReadPkt(&packet, params.rp_timeout) == SUCCESS) {
	    if (packet.type == REOT) {
		break;
	    }
	    else if (packet.type == RDATA) {
		/* Avoid receiving more than MAX_INFO_SIZE bytes. */
		received_this_time = packet.length;
		if (total_received + received_this_time > MAX_INFO_SIZE) {
		    received_this_time = MAX_INFO_SIZE - total_received;
		    receive_overflow = TRUE;
		}
		for (i = 0; i < received_this_time; i++) {
		    fputc(packet.data[i], stderr);
		    if (upgrade_f != NULL) {
			fputc(packet.data[i], upgrade_f);
		    }
		}
		if (receive_overflow) {
		    error(MSG_ERROR, "Overflow while receiving upgrade info");
		    return FAIL;
		}
	    }
	    else {
		error(MSG_ERROR, "Received unexpected packet type");
		return FAIL;
	    }
	}
	else {
	    if (errno == EINTR) {
	        error(MSG_ERROR, "Timeout while receiving upgrade info");
	        return FAIL;
	    }
	    else {
	        error(MSG_ERROR, "Fatal error trying to read upgrade info");
	        return FAIL;
	    }
	}
    }

    if (upgrade_f != NULL) {
	fclose(upgrade_f);
	error(MSG_WARNING, "Upgrade info was saved in file '%s'.",
	      params.upgrade_file);
    }
    
    error(MSG_WARNING, "Reception complete.");
    return SUCCESS;
}



/*

   FUNCTION: CantNotifyBetaVersion

   PURPOSE:

   This function is called by the newer of two versions of Nautilus
   participating in a call, but only when the older version is a
   pre-1.0 version.  A message is displayed on the screen explaining
   this, and suggests contacting the pre-1.0 user by some other means
   to let them know how to upgrade.  This message is also saved in the
   file with name UPGRADE_FILE, if it does not already exist, so that
   it may be more easily sent to the pre-1.0 user.

   ARGUMENTS: None

   RETURN VALUE:

   SUCCESS if the message was saved in the file, FAIL otherwise.
   
   SIDE EFFECTS: None
*/

int
CantNotifyBetaVersion()
{
    FILE         *upgrade_f;

    error(MSG_ERROR, "The other Nautilus user has a version earlier than 1.0,\n\
and it is incompatible with your version.  You should ask them to upgrade\n\
their version.  Send them the following instructions...\n\
\n\
\n");

    error(MSG_ERROR, upgrade_info);

    upgrade_f = OpenUpgradeFile();
    if (upgrade_f != NULL) {
	fputs(upgrade_info, upgrade_f);
	fclose(upgrade_f);
	fprintf(stderr,
"\n\nThe upgrade information was also saved in the file '%s'.\n",
		params.upgrade_file);
	fflush(stderr);
	return SUCCESS;
    }
    
    return FAIL;
}
