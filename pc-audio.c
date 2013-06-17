/*************************************************************************
      PC_AUDIO.C for Sound Blaster cards only.

      This routine works with Sound Blaster 2.0, SB-Pro, and SB-16 cards
      only. It will NOT work with the old, original Sound Blaster 1.xx
      versions because the DMA operations are different from the newer
      cards. The old cards stopped shipping in 1990.

      Note: There is no software volume control on the 8-bit Sound Blaster 2.0
      cards. Volume control is by external controls only. The newer
      SB-Pro and SB-16 cards have internally adjustable volume controls.

      Some portions of this code are derived from "DMA.C", a test program
      from Creative Labs, written by Tom Bouril. Other portions are derived
      from the Creative Labs Sound Blaster Software Developer Kit Rev. 2.

		      *** NOTICE  NOTICE  NOTICE ***

      The "BLASTER" environment variable *MUST* be present for this code
      to work. If it isn't present in you AUTOEXEC.BAT file, run the
      program SETENV.EXE supplied by Creative Labs with all models of
      Sound Blaster Cards. This will place the appropriate BLASTER
      variable in your AUTOEXEC.BAT file for you.

		  Patrick Mullarky   8/94

**************************************************************************/

/* SCCS ID: @(#)pc-audio.c 1.1 96/02/29 */

#include <bios.h>
#include <conio.h>
#include <dos.h>
#include <memory.h>
#include <malloc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "nautilus.h"
#include "pc-audio.h"
		     
extern struct param_t   params;     /* operating parameters */

/*************************************************************************
*
* FUNCTION:  InitAudio(FrameSize)
*
* DESCRIPTION: Initialize the Audio system. Save off the frame size, and
*              calculate how many frames will fit into the DMA buffer.
*              Then read the BLASTER environment variable (required) to
*              actually initialize the card, DMA channel, IRQ, etc. Also
*              save original Blaster card values.
*
* parameter "verbose" is not used, but is here to parallel the unix code.
* The "#if DEBUG > 1" could be replaced with an "if verbose".
*
**************************************************************************/

int InitAudio(int SampleRate, int FrameSize, int verbose)
{
    UINT8 *DMABuffer = gDMABuffer; /* this has to be local .. don't know why */

   /* if DMABuffer isn't local, the following doesn't work (!) */

#ifdef SMALL
   struct _SREGS segregs;
   _segread(&segregs);   /* only required if using "small" model! */
    gBufPhysAddr = ((UINT32)segregs.ds<<4)
	       + (UINT32)FP_OFF(DMABuffer);
#else
    gBufPhysAddr = ((UINT32)FP_SEG(DMABuffer)<<4)
	       + (UINT32)FP_OFF(DMABuffer);
#endif

    gFrameSize = FrameSize;
	/* how many frames will fit in the buffer? */
    gMaxFrames = DMA_BUF_SIZE / FrameSize;
       /* actual logical size of buffer */
    gBufferSize = gMaxFrames * FrameSize ;  /* actual truncated size */

      /* The following is straight out of the Sound Blaster Hardware */
      /* Reference Manual, 2nd Edition.  It's odd, but it works. */
   gByteTimeConstant = (UINT8) ((65536-(256000000/SampleRate)) >> 8);
   gByteTimeConstant++;		/* wwd */

   gIOPointer  = gDMABuffer;
   gDMAPaused   = FALSE;
   gDMARunning = FALSE;

    gFrameCount = 0;      /* Altered by Read/Write Audio, DMA_Isr */

  /*--- GET ENVIRONMENT VALUES -----------------------------------*/
  /*--------------------------------------------------------------*/
   if (GetBlasterEnv() == FAIL)
   {
        if (params.msdos.snd_iobase == -1)
        {
            puts("Warning: Using default value for sound card I/O base address.\n");
            params.msdos.snd_iobase = DEFAULT_SND_IO;
	}
	if (params.msdos.snd_irq == -1)
	{
	    puts("Warning: Using default value for sound card IRQ number.\n");
	    params.msdos.snd_irq = DEFAULT_SND_IRQ;
	}
	if (params.msdos.snd_dma == -1)
	{
	    puts("Warning: Using default value for sound card DMA channel.\n");
	    params.msdos.snd_dma = DEFAULT_SND_DMA;
	}
   }
   gIOPort = params.msdos.snd_iobase;
   gIRQNumber = params.msdos.snd_irq;
   gDMAChan = params.msdos.snd_dma;

#if DEBUG > 1
  /*--- PRINT OUT INFO -------------------------------------------*/
  /*--------------------------------------------------------------*/
  printf("    DMA Buffer             = %lp\n", gDMABuffer);
  printf("    DMA Buffer Phys. Addr. = %-8X   (hex)\n",  gBufPhysAddr);
  printf("    8-bit DMA channel      = %-5d     (decimal)\n",   gDMAChan);
  printf("    I/O port address       = %-3x       (hex)\n",       gIOPort);
  printf("    IRQ number             = %-2d        (decimal)\n\n", gIRQNumber);
#endif

  /*--- RESET THE DSP ----------------------------------------*/
  /*----------------------------------------------------------*/
   if(ResetDSP(gIOPort) == FAIL)
   {
	puts("Sound card hardware or compatability failure.");
	return(FAIL);
   }


  /*--- SAVE CURRENT ISR FOR IRQNumber, THEN GIVE IRQNumber NEW ISR ---*/
  /*-------------------------------------------------------------------*/
  gIRQSave = _dos_getvect(gIRQNumber + 8);
  _dos_setvect(gIRQNumber + 8, DMA_Isr);


  /*--- SAVE CURRENT INTERRUPT MASK AND SET NEW INTERRUPT MASK -------*/
  /*------------------------------------------------------------------*/

  gMaskSave = inp((INT16) PIC_MASK);
  gIRQMask = ((INT16) 1 << gIRQNumber);     /* Shift a 1 left IRQNumber of bits */
  outp(PIC_MASK, (gMaskSave & ~gIRQMask));  /* Enable previous AND new interrupts */

  /*--- PROGRAM THE DMA, DSP CHIPS -----------------------------------*/
  /*------------------------------------------------------------------*/
  if (InitDMA(gBufPhysAddr, DMA8_READ) == FAIL)
	return (FAIL);

  /*--- Save Blaster state   -----------------------------------------*/
  /*------------------------------------------------------------------*/

  outp(gIOPort + MIXER_ADDR, EIGHT_BIT_VOLUME);
  g8BitVol = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, MIC_VOLUME);
  gMicVol = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, VOICE_VOLUME);
  gVoiceVol = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, MASTER_VOLUME);
  gMasterVol = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN);
  gInputGainLeft = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN+1);
  gInputGainRight = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN);
  gOutputGainLeft = inp(gIOPort + MIXER_DATA);

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN+1);
  gOutputGainRight = inp(gIOPort + MIXER_DATA);

   /* Set Default Input/Output Gains (SB16) */

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN);
  outp(gIOPort + MIXER_DATA, DEFAULT_OUTPUT_GAIN);

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN+1);
  outp(gIOPort + MIXER_DATA, DEFAULT_OUTPUT_GAIN);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN);
  outp(gIOPort + MIXER_DATA, DEFAULT_INPUT_GAIN);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN+1);
  outp(gIOPort + MIXER_DATA, DEFAULT_INPUT_GAIN);

  atexit(CloseAudio);   /* Set up exit call - wwd */

  return(0);
}

/***************************************************************************
*
* FUNCTION: AudioFlow()
*
* DESCRIPTION: Change the Audio direction to TRANSMIT or RECEIVE. When
*              receiving, the DMA direction is "write", and will pause
*              if no data is available (underrun condition). When transmitting
*              the DMA direction is "read". If the "read" buffer overfills,
*              the buffer will wrap around and reading will continue.
*
* RETURNS: Returns SUCCESS. (No error).
*
***************************************************************************/

int AudioFlow(enum flow direction)
{
    switch(direction)
    {
	case TRANSMIT:

		/* reset DSP */
	    ResetDSP(gIOPort);

	      /* output the calculated time constant */
	DSPOut(gIOPort, DSP_TIME_CONSTANT);
	    DSPOut(gIOPort, gByteTimeConstant);

		/* turn off speakers, turn on microphone */
	    DSPOut(gIOPort, DSP_SPEAKER_OFF);
	    if (!strcasecmp(params.audio.mic_sens, "low"))
	    	SetMixer(MIC_LEVEL_LOW, 0, 0);
	    else if (!strcasecmp(params.audio.mic_sens, "high"))
	    	SetMixer(MIC_LEVEL_HIGH, 0, 0);
	    else
	    	SetMixer(MIC_LEVEL_MEDIUM, 0, 0);

		/* turn on reading flag */
	    gReadingFlag = FALSE;

		/* reset buffer pointer */
	    gIOPointer = gDMABuffer;
	gFrameCount = 0;

		/* set DMA flags */
	    gDMARunning = TRUE;
	    gDMAPaused = FALSE;

		/* set DMA to "write" */
	    InitDMA(gBufPhysAddr, DMA8_WRITE);

		/* program and start DMA for input */
	    DSPOut(gIOPort, DSP_BLOCK_SIZE);
	    DSPOut(gIOPort, ((gFrameSize-1) & 0x00FF)); /* LO byte */
	    DSPOut(gIOPort, ((gFrameSize-1) >> 8));     /* HI byte */
	    DSPOut(gIOPort, DSP_8BIT_INPUT);


	    break;

	case RECEIVE:

		/*reset DSP */
	    ResetDSP(gIOPort);

	      /* output the calculated time constant */
	DSPOut(gIOPort, DSP_TIME_CONSTANT);
	DSPOut(gIOPort, gByteTimeConstant);

	    /* turn on speakers, turn off mic */
	if (!strcasecmp(params.audio.out_volume, "low"))
	    SetMixer(0, VOICE_LEVEL_LOW, MASTER_LEVEL_LOW);
	else if (!strcasecmp(params.audio.out_volume, "high"))
	    SetMixer(0, VOICE_LEVEL_HIGH, MASTER_LEVEL_HIGH);
	else
	    SetMixer(0, VOICE_LEVEL_MEDIUM, MASTER_LEVEL_MEDIUM);
	DSPOut(gIOPort, DSP_SPEAKER_ON);

		/* turn on reading flag */
	    gReadingFlag = TRUE;

		/* reset buffer pointer */
	    gIOPointer = gDMABuffer;
	gFrameCount = 0;

		/* set DMA flags   Note: DMA is started by WriteAudio() */
	    gDMARunning = FALSE;
	    gDMAPaused = FALSE;

		/* set DMA to "read" */
	    InitDMA(gBufPhysAddr, DMA8_READ);
	    break;
    }
    return(SUCCESS);
}




/***************************************************************************
*
* FUNCTION: WriteAudio()
*
* DESCRIPTION: Queue frames into the DMA buffer, and increment the frame
*              counter (frames now in the DMA buffer yet to be played).
*              Reset the buffer pointer if it is at the end. If the frame
*              count went from zero to one, (re)start the DMA.
*
* RETURNS:  Returns SUCCESS if successful, FAIL if the desired number of
*           frames will overflow the DMA Buffer.
*
***************************************************************************/

int WriteAudio(UINT8 *Data, int Frames)
{

    if(gFrameCount + Frames <= gMaxFrames)
    {
	while(Frames--)
	    {
		    /* reset buffer pointer if necessary */
		if(gIOPointer >= gDMABuffer+gBufferSize) gIOPointer = gDMABuffer;

		    /* copy data to DMA buffer */
		memcpy(gIOPointer, Data, gFrameSize);

		gIOPointer += gFrameSize;
		Data += gFrameSize;

		    /* increment frame count (guarded) */
		_asm cli;
		gFrameCount++;
		_asm sti;
	    }
    }
    else return(FAIL);

    if (gDMARunning == FALSE)   /* Are we running? If not, start DMA */
    {
	    /* Start DMA if frame is the very first frame to play */
	    /* using the "8-bit Mono Auto-initialize Transfer" mode. */
	    DSPOut(gIOPort, DSP_BLOCK_SIZE);
	    DSPOut(gIOPort, ((gFrameSize-1) & 0x00FF)); /* LO byte */
	    DSPOut(gIOPort, ((gFrameSize-1) >> 8));     /* HI byte */
	    DSPOut(gIOPort, DSP_8BIT_OUTPUT);
	    gDMARunning = TRUE;
    }

    else if(gDMAPaused == TRUE)
    {
	   /* if paused (underrun condition), restart DMA */
	DSPOut(gIOPort, DSP_RESTART_DMA);
    }
    return(SUCCESS);
}

/***************************************************************************
*
* FUNCTION: ReadAudio()
*
* DESCRIPTION: Get frames from the DMA buffer, and decrement the frame
*              counter (frames now in the DMA buffer yet to be read).
*              Reset the buffer pointer if it is at the end.
*                   Will block until required number of frames are read.
*
* RETURNS:  Returns SUCCESS. A timeout function is needed here.
*
***************************************************************************/

int ReadAudio(UINT8 *Data, int Frames)
{
	   /* wait here for frames to come in if not yet available. (block) */

    if(gFrameCount < Frames)
    {   
	while (gFrameCount < Frames );
    }

    while(Frames--)
    {
	    /* copy data to DMA buffer */
	    memcpy(Data, gIOPointer, gFrameSize);
		/* increment the DMA pointer */
	    gIOPointer += gFrameSize;
		/* reset DMA pointer if necessary */
	    if(gIOPointer >= gDMABuffer+gBufferSize)
	    gIOPointer = gDMABuffer;
		/* also bump the input pointer */
	    Data += gFrameSize;

		/* decrement frame count (guarded) */
	    _asm cli;
	    gFrameCount--;
	    _asm sti;
    }

    return(SUCCESS);
}

/***************************************************************************
*
* FUNCTION: DrainAudio()
*
* DESCRIPTION: Block until previously queued audio data has been played.
*
***************************************************************************/

void DrainAudio(void)
{
    while (gFrameCount);
}


/*************************************************************************
*
* FUNCTION: DMA_Isr()
*
* DESCRIPTION:  Interrupt service routine.  Every time the DSP chip finishes
*               a frame, this routine is invoked. If writing, and the frame
*               count goes to zero (underrun), pause the DMA. If reading
*               and the frame count exceeds maxframes, the buffer will
*               simply wrap around (with no warning).
*
*************************************************************************/
void interrupt far DMA_Isr(void)
{
    _asm
    {
	    mov     dx,gIOPort
	    add     dx,DSP_DATA_AVAIL
	    in      al,dx               ; Acknowledge interrupt

	    mov     al,20H
	    out     20h,al              ; Set general EOI

	    cmp     gReadingFlag,TRUE   ; are we reading?
	    jz      writing             ; if not, then writing...

	    mov     ax,gFrameCount      ; compare frame count to max
	    cmp     ax,gMaxFrames
	    je      int_exit            ; if already at max, just exit
	    inc     gFrameCount         ; else increment frame count
	    jmp     int_exit

      writing:
	    sub     gFrameCount,1       ; writing, so decrement frame count
	    jnz     int_exit            ; if count not zero, simply exit

	    mov     dx,gIOPort          ; if nothing left to write,
	    add     dx,DSP_WRITE_PORT   ; pause the DMA

      wait_loop1:
	    in      al,dx               ; wait for SB not busy
	    and     al,0x80
	    jne     wait_loop1

	    mov     al,DSP_PAUSE_DMA    ; pause DMA
	    out     dx,al
	    mov     gDMAPaused,TRUE     ; set "paused" flag

      int_exit:                         ; all done!
    }
}


/*************************************************************************
*
* FUNCTION: InitDMA()
*
* DESCRIPTION: Initialize the DMA channel in the PC.
*
*              NOTE: The DMA chip is always programmed for auto-init mode
*                    (command 0x58 or 0x54)!
*
*************************************************************************/
char InitDMA(UINT32 BufPhysAddr, INT16 Direction)

{
  INT16  DMAAddr,
       DMACount,
       DMAPage,
       Offset,
       Page;

  /*--- GET DMA ADDR., COUNT, AND PAGE FOR THE DMA CHANNEL USED ----------*/
  /*----------------------------------------------------------------------*/

   switch(gDMAChan)
   {
      case 0:
	    DMAAddr  = DMA0_ADDR;
	    DMACount = DMA0_COUNT;
	    DMAPage  = DMA0_PAGE;
      break;

      case 1:
	    DMAAddr  = DMA1_ADDR;
	    DMACount = DMA1_COUNT;
	    DMAPage  = DMA1_PAGE;
      break;

      case 3:
	    DMAAddr  = DMA3_ADDR;
	    DMACount = DMA3_COUNT;
	    DMAPage  = DMA3_PAGE;
      break;

      default:
	    puts("Invalid 8-bit DMA channel in BLASTER environment string");
      return(FAIL);
   }

  /*--- PROGRAM THE DMA CHIP ---------------------------------------------*/
  /*----------------------------------------------------------------------*/
  Page   = (INT16) (BufPhysAddr >> 16);
  Offset = (INT16) (BufPhysAddr & 0xFFFF);

  outp(DMA8_MASK_REG, (gDMAChan | 4));     /* Disable DMA while prog. */
  outp(DMA8_FF_REG, 0);                   /* Clear the flip-flop */

  outp(DMACount, ((gBufferSize - 1) & 0xFF)); /* LO byte of count */
  outp(DMACount, ((gBufferSize - 1) >> 8));   /* HI byte of count */

  outp(DMAPage, Page);             /* Physical page number */
  outp(DMAAddr, (Offset & 0xFF));  /* LO byte address of buffer */
  outp(DMAAddr, (Offset >> 8));    /* HI byte address of buffer */

  outp(DMA8_MODE_REG, (gDMAChan  | Direction)); /* 8-bit auto-init. */

      /* Done programming the DMA, enable it */
  outp(DMA8_MASK_REG, gDMAChan);

  return(SUCCESS);
}


/**************************************************************************
*
* FUNCTION: GetBlasterEnv()
*
* DESCRIPTION : Get the BLASTER environment variable and search its
*               string for the DMA channel, I/O address port, and
*               IRQ number.
*
* RETURN:  Returns SUCCESS if OK. If "BLASTER" environment variable
*          is absent, return FAIL.
*
**************************************************************************/
char GetBlasterEnv(void)
{
  char *blaster,*p;

    /* Attempt to read environment variable (forced to upper case) */
    if(getenv("BLASTER") != NULL)
      blaster = strupr(strdup(getenv("BLASTER")));  /* does a "malloc" */
    else return(FAIL);

    /* Now parse the BLASTER variable */
    p = strtok(blaster," \t");

    while(p)
    {
      switch(*p)
      {
	case 'A':
	  params.msdos.snd_iobase = (INT16) strtol(p+1, NULL, 16);
	  break;

	case 'I':
	  params.msdos.snd_irq = (INT16) atoi(p+1);
	  break;

	case 'D':
	  params.msdos.snd_dma = (INT16) atoi(p+1);
	  break;
       }
      p = strtok(NULL," \t"); /* get next token, if any. */
    }
    free(blaster);
    return(SUCCESS);
}



/*************************************************************************
*
* FUNCTION: DSPOut()
*
* DESCRIPTION: Writes the value passed to this function to the DSP chip.
*
*************************************************************************/
void DSPOut(INT16 IOBasePort, INT16 WriteValue)
{
    _asm
    {
	mov     dx,IOBasePort
	add     dx,DSP_WRITE_PORT
  wait_loop:
	in      al,dx
	and     al,0x80
	jne     wait_loop
	mov     ax,WriteValue
	out     dx,al
    }
}


/*************************************************************************
*
* FUNCTION: ResetDSP()
*
* DESCRIPTION: Self explanatory
*
*************************************************************************/
char ResetDSP(INT16 IOBasePort)
{
   INT32 goal;

   outp(IOBasePort + DSP_RESET, (INT16) 1);

   goal = 10 + clock();  /* wait 10 milliseconds */
   while( goal > clock() ) ;

   outp(IOBasePort + DSP_RESET, (INT16) 0);

      /* Wait until data is available */
   while ((inp(IOBasePort + DSP_DATA_AVAIL) & 0x80) == 0);

   if (inp(IOBasePort + DSP_READ_PORT) == DSP_READY)
      return(SUCCESS);
   return(FAIL);
}



/**************************************************************************
*
* FUNCTION: SetMixer()
*
* DESCRIPTION: Self explanatory
*
**************************************************************************/
void SetMixer(INT16 MicVolume, INT16 VoiceVolume, INT16 MasterVolume )
{
    outp(gIOPort + MIXER_ADDR, EIGHT_BIT_VOLUME);
    outp(gIOPort + MIXER_DATA, MasterVolume);

    outp(gIOPort + MIXER_ADDR, MIC_VOLUME);
    outp(gIOPort + MIXER_DATA, MicVolume);

    outp(gIOPort + MIXER_ADDR, VOICE_VOLUME);
    outp(gIOPort + MIXER_DATA, VoiceVolume);

    outp(gIOPort + MIXER_ADDR, MASTER_VOLUME);
    outp(gIOPort + MIXER_DATA, MasterVolume);

    return;
}

/*************************************************************************
* CloseAudio:   Restore the original IRQ mask and vector and free memory.
*
* WARNING: If this routine isn't called before exiting, DOS will
*          eventually crash because the DMA stuff is still enabled!
*
*************************************************************************/
void CloseAudio(void)
{
  ResetDSP(gIOPort);    /* Shut down the DSP */

  outp(DMA8_MASK_REG, (gDMAChan | 4));     /* Disable DMA */

  outp(PIC_MASK, gMaskSave);  /* Restore old Int. Controller Mask */

  _dos_setvect(gIRQNumber + 8, gIRQSave);  /* Restore old IRQ vector */

   /* Restore the old Mixer values */

  outp(gIOPort + MIXER_ADDR, EIGHT_BIT_VOLUME);
  outp(gIOPort + MIXER_DATA, g8BitVol);

  outp(gIOPort + MIXER_ADDR, MIC_VOLUME);
  outp(gIOPort + MIXER_DATA, gMicVol);

  outp(gIOPort + MIXER_ADDR, VOICE_VOLUME);
  outp(gIOPort + MIXER_DATA, gVoiceVol);

  outp(gIOPort + MIXER_ADDR, MASTER_VOLUME);
  outp(gIOPort + MIXER_DATA, gMasterVol);

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN);
  outp(gIOPort + MIXER_DATA, gOutputGainLeft);

  outp(gIOPort + MIXER_ADDR, OUTPUT_GAIN+1);
  outp(gIOPort + MIXER_DATA, gOutputGainRight);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN);
  outp(gIOPort + MIXER_DATA, gInputGainLeft);

  outp(gIOPort + MIXER_ADDR, INPUT_GAIN+1);
  outp(gIOPort + MIXER_DATA, gInputGainRight);

}

/**************************************************************************
*
* FUNCTION: PlayVoice()  Play a voice file (logon.v or ring.v).
*
* DESCRIPTION: Self explanatory
*
* RETURNS: SUCCESS or FAIL
*
* 97/07/22 D. Miller        Return success of opening file
* 97/05/01 D. Miller        Renamed from 'VoiceLogon()' to 'PlayVoice()'
*
**************************************************************************/

int
PlayVoice(char *logonfile)
{
    char *FrameBuffer;

    FILE *FileToPlay;
   
     int  EndOfFile,
    BytesLeftToPlay;

    EndOfFile = FALSE;
   
    AudioFlow(RECEIVE);  /* turn on sound card for playing to speaker */
   
    if( (FrameBuffer = (char *)malloc(gFrameSize)) == NULL)
    {
       error(MSG_FATAL, "PC-Audio(): memory allocation error");
       return FAIL;
    }
   
  /*--- OPEN FILE TO BE PLAYED -------------------------------*/
  /*------------------- If it's not there, just exit ---------*/

    if ((FileToPlay = fopen(logonfile, "rb")) == NULL)
    {    
	    free(FrameBuffer);    /* if it isn't there, just quit */
	    return FAIL;
    }

    while(!EndOfFile)
    {
	      /* if DMA buffer not full, add another frame */
	if(gFrameCount < gMaxFrames) 
	    {
		BytesLeftToPlay = fread(FrameBuffer, 1, gFrameSize, FileToPlay);
		if (BytesLeftToPlay < gFrameSize)  /* bytes read < frame size. */
		    EndOfFile = TRUE;
		WriteAudio(FrameBuffer, 1); /* will start DMA if not running */
	    }
    }
	
    while(gFrameCount) ;        /* wait for last frame output to SB */

    fclose(FileToPlay);
    free(FrameBuffer);
    return SUCCESS;
}

/****************************************************************/
/****************************************************************/
/*        End of code by Pat Mullarky                           */
/****************************************************************/
/****************************************************************/



/****************************************************************
*
* The following have been written by others, and are needed for
* running Nautilus under DOS.
*
*
*
****************************************************************/

/****************************************************************/
/*               Raw2Std()                                      */
/****************************************************************/

/* audio data conversion to/from 16 bit signed or 8 bit unsigned */
void
Raw2Std(UINT8 *src, INT16 *dest, INT16 leng)
{
      for(; leng--; )
	*dest++ = *src++ -128 << 8;
}

/****************************************************************/
/*               Std2Raw()                                      */
/****************************************************************/

void
Std2Raw(INT16 *src, UINT8 *dest, INT16 leng)
{
    for (; leng--; )
      *dest++ = (int)(*src++ >>8) +128;
}

/****************************************************************/
/*                ptt()  &  quit()                              */
/****************************************************************/

static INT16 qflag;

/* detect ptt change.  return char if keypress, false otherwise */
/* set qflag if 'q' or 'Q'. */
int ptt(void)
{
  INT16  c;

  /* see if there are any characters waiting to be read */
  if (kbhit()) {
    c = getch();
    if (c == 'q' || c == 'Q')
      qflag = 1;
    else
      return c;      /* return actual character */
  }
  return 0;
}


/* detect user abort.  return true if user desires to abort, false otherwise */
int quit(void)
{
  int tmp;
  tmp = qflag;
  qflag = 0;       /* clear out flag before exit; */
  return tmp;
}

/****************************************************************/
/*                  safeout()                                   */
/****************************************************************/

void safeout(INT8 *str)      /* mother says do it this way */
{
    union REGS inregs, outregs;
    inregs.h.ah = 0x0e;
    while (*str) {
	inregs.h.al = *str++;
	int86( 0x10, &inregs, &outregs );
  }
}

/****************************************************************/
/*                  breaker()                                   */
/****************************************************************/

void breaker(INT16 sigtype)
{
  INT16 c;
  signal(SIGINT, SIG_IGN);
  safeout("\nuser break detected- abort Nautilus (y/n) ?");
  while(1)
  {
    if( (c=_bios_keybrd(_KEYBRD_READ) & 0xff) =='y') {
	safeout("\n\r");
	exit(-1);
    }
    else if( c == 'n') {
      signal(SIGINT, breaker);
      safeout("\n");
      break;
    }
  }
}

/****************************************************************/
/*                   SysInit()                                  */
/****************************************************************/

int SysInit(void)
{

  if((signal(SIGINT, breaker))==SIG_ERR) {
    fputs("pc:SysInit: error installing break handler\n", stderr);
    return FAIL;
  }
  return SUCCESS;
}

/****************************************************************/
/*                      Clock()                                 */
/****************************************************************/

long        /* return system time in secs */
Clock(void)
{
  return (long) time(NULL);
}

/****************************************************************/
/*                      sleep()                                 */
/****************************************************************/

void
sleep(unsigned secs)
{
    if(secs) {
	long base = clock();
	base += secs*CLOCKS_PER_SEC;
	while (clock() < base)
	    ;
    }
}

/****************************************************************/
/*                      Timer()                                 */
/****************************************************************/

INT32
Timer(int init)
{
    static long base;

    if (init)
        base = clock();

    return clock() - base;
}

/****************************************************************/
/*                      GetPassPhrase()                         */
/****************************************************************/

int
GetPassPhrase(char *pw, int maxlen, char *msg)
{
    int i, c, done;

    /* prompt user */
    fputs(msg, stdout);

    /* get passphrase */
    i = done = 0;
    do {
    	c = getch();
    	switch (c) {
    	case '\r':
    	case '\n':
    	    pw[i] = '\0';
    	    done = 1;
    	    break;
	case '\010':
	    if (i > 0)
	    	i--;
	    break;
	default:
    	    if (i < maxlen)
    	    	pw[i++] = c;
	    break;
    	}
    } while (!done);

    fputs("\n", stdout);

    return 0;
}

/****************************************************************/
/*      required function aliases strcasecmp and strncasecmp    */
/****************************************************************/


strncasecmp(char *s1, char *s2, int n)
{
    return strnicmp(s1, s2, n);
}

strcasecmp(char *s1, char *s2)
{
    return stricmp(s1, s2);
}


/****************************************************************/
/****************************************************************/
/*                                                              */
/*            END OF MODULE PC-AUDIO.C                          */
/*                                                              */
/****************************************************************/
/****************************************************************/
