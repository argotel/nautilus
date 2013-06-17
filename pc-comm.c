/****************************************************************************/
/****************************************************************************/
/*      PC-COMM.C        Serial Communications module for Nautilus

    This module is a simple serial-port I/O module which will run 8250
    and 16550 UART's. It uses 1K buffers for input and output. It is
    capable of handling 28,800 kbps with little overhead.
    
    ASSUMPTIONS:
     
    The COM port used must be selected from the following:
    
    COM1, at I/O address 0x3F8, using IRQ4
    COM2, at I/O address 0x2F8, using IRQ3
    COM3, at I/O address 0x3E8, using IRQ4
    COM4, at I/O address 0x2E8, using IRQ3
    
    These are the "usual" COM ports in PC-Compatible computers.
    
    NOTE: Use COM3 or COM4 with caution. Note the IRQ's assigned
    to COM3 and COM4, above. If You use COM3, there shouldn't be
    any activity on COM1 while COM3 is running, or the interrupts
    will conflict. It is safest to use only COM1 or COM2 for the
    modem running the Nautilus program.
    
    Patrick Mullarky   October, 1994
    
*/
/****************************************************************************/
/****************************************************************************/

/* SCCS ID: @(#)pc-comm.c 1.1 96/02/29 */

    /* INCLUDES */
    
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>
#include "nautilus.h"
#include "pc-comm.h"
                     
    /* GLOBAL VARIABLES */ 
    
/* debug */
    
UINT16  UART_addr,
        UART_irq,
        volatile RX_count,
        volatile TX_count;
        
INT16   TX_paused_flag;
        
UINT8   RX_buffer[BUFSIZE],
        TX_buffer[BUFSIZE],
        
        *RX_in_pointer,
        *TX_in_pointer,
        *RX_out_pointer,
        *TX_out_pointer,
        *RX_end_pointer,
        *TX_end_pointer;
        
void    (interrupt far *serial_irq_save)();  /* save location for old vector */
UINT8       PIC_save;

extern struct param_t   params;     /* operating parameters */

                                   /* save old PIC value */
/****************************************************************************/
/****************************************************************************/
/*                                                                          */
/*                                                                          */
/* FUNCTION                                                                 */
/*                                                                          */
/*          InitPort(char *comm_port, UINT16 speed)                         */
/*                                                                          */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*      Initialize the serial port:                                         */
/*                                                                          */
/*      Check for UART existence and type.                                  */
/*          If 16550-type, enable FIFO logic.                               */
/*                                                                          */
/*      Initialize UART baud-rate, DTR, and RTS.                            */
/*                                                                          */
/*      Initialize interrupt vector, PIC chip, and interrupt enables.       */
/*                                                                          */
/* RETURNS                                                                  */
/*      Returns SUCCESS if OK, else FAIL. Failure will occur if the check   */
/*      for existence and type fails.                                       */
/*                                                                          */
/*                                                                          */
/****************************************************************************/

INT16
InitPort(char *comm_port, UINT16 speed)
{

    enum {NONE, NS8250, NS16550A} chip_type;

    int divisor;
    
        /* Initialize the buffer stuff */
        
    TX_in_pointer = TX_out_pointer = TX_buffer;
    TX_end_pointer = TX_buffer + BUFSIZE;
    
    RX_in_pointer = RX_out_pointer = RX_buffer;
    RX_end_pointer = RX_buffer + BUFSIZE;
    
    RX_count = TX_count = 0;
    
    TX_paused_flag = TRUE;
    
    /* Simple parse of comm port argument */

    UART_addr = 0;
    UART_irq =0;

    if(strcasecmp(comm_port, "COM1")==0)
    {
        if (params.msdos.com_iobase[0] != -1)
            UART_addr = params.msdos.com_iobase[0];
	else
            UART_addr = COM_1;
        if (params.msdos.com_irq[0] != -1)
            UART_irq = params.msdos.com_irq[0];
	else
            UART_irq = COM1_IRQ;
    }

    if(strcasecmp(comm_port, "COM2")==0)
    {
        if (params.msdos.com_iobase[1] != -1)
            UART_addr = params.msdos.com_iobase[1];
	else
            UART_addr = COM_2;
        if (params.msdos.com_irq[1] != -1)
            UART_irq = params.msdos.com_irq[1];
	else
            UART_irq = COM2_IRQ;
    }
                    
    if(strcasecmp(comm_port, "COM3")==0)
    {
        if (params.msdos.com_iobase[2] != -1)
            UART_addr = params.msdos.com_iobase[2];
	else
            UART_addr = COM_3;
        if (params.msdos.com_irq[2] != -1)
            UART_irq = params.msdos.com_irq[2];
	else
            UART_irq = COM3_IRQ;
    }
    
    if(strcasecmp(comm_port, "COM4")==0)
    {
        if (params.msdos.com_iobase[3] != -1)
            UART_addr = params.msdos.com_iobase[3];
	else
            UART_addr = COM_4;
        if (params.msdos.com_irq[3] != -1)
            UART_irq = params.msdos.com_irq[3];
	else
            UART_irq = COM4_IRQ;
    }
    
    if(UART_addr == 0)
    {
        puts("Unknown COM port!");
        exit(1);                             /* was exit(0); dm 06/16/97 */
    }

    /*  Test for existence  */
    

    writeLCR(0x55);                          /* check for existence */
    if(readLCR() != 0x55) chip_type = NONE;
    writeLCR( 0 );
    if(readLCR() != 0) chip_type = NONE;

    writeFCR( 1 );                           /* check for FIFO's */
    if( (readIIR() & 0xC0) == 0xC0) chip_type = NS16550A;
    else chip_type = NS8250;

    if(chip_type == NONE) return FAIL;

    writeFCR( 0 );

    #if DEBUG
         printf("Serial Chip:    ");
    if(chip_type == NS8250)
         printf("8250\n");
    else printf("16550\n");
    #endif


        /*   Set up divisor, RTS, DTR, and Out2 */
    
    divisor = (int)( 115200L / speed );
    
    writeLCR( 0x80 );                   /* Set DLAB */
    writeDLL(divisor & 0xFF);           /* lower divisor half */
    writeDLM((divisor >>8) & 0xFF);     /* higher divisor half */
    
    writeLCR(0x03);  /* Clear DLAB, set data to 8 bits, no parity, 1 stop bit */
    
    writeMCR(0);  /* clear DLAB */
    
    
    /* Set up the chosen interrupt vector after saving old...  */
    
    serial_irq_save = _dos_getvect(8 + UART_irq);
                           
    if (chip_type == NS8250)           /* Choose your weapon.... */
        _dos_setvect (8 + UART_irq, irq_8250);
    else
    {
        _dos_setvect (8 + UART_irq, irq_16550);
        writeFCR(0xC1);  /* Enable FIFO's, trigger = 14 chars. */
    }
        
    
       /* Set Programmable Interrupt Controller (PIC) mask */
    
    PIC_save = inp(PIC_MASK);   /* save original value */
    
    if( (UART_addr == COM_1) || (UART_addr == COM_3) )    /* IRQ4 */
        outp(PIC_MASK, (PIC_save & 0xEF));
    else
        outp(PIC_MASK, (PIC_save & 0xF7));                /* IRQ3 */
        
        /* Get ready to go....  */
        
    writeMCR(0x10);  /* set loopback mode */
    writeTHR(0);     /* write one dummy character (loopback mode) */
    
    while ((readLSR() & 0x40) == 0);   /* wait for transmitter empty */

    writeMCR(0x0B);  /* set Out2+DTR+RTS */
    readRBR();       /* read any pending RX character */
    writeIER(0x0D);  /* enable all but TX interrupts */

    atexit(ClosePort);
    
    return(SUCCESS);        
}    
 
/****************************************************************************/
/*                                                                          */
/*  FUNCTION  irq_16550()     16550A Interrupt Service Routine              */
/*                                                                          */
/*  DESCRIPTION                                                             */
/*      Handles interrupts from the 16550A UART. Fills or empties FIFO's    */
/*      as required. Bogus interrupts are simply dismissed.                 */
/*                                                                          */
/*      If the TX buffer becomes empty, the TX interrupt is masked off. It  */
/*      will be re-started by WritePort().                                  */
/*                                                                          */
/*      If the RX buffer overfills, it simply wraps around without warning. */
/*                                                                          */
/*                                                                          */
/*  Note: "case" statements do not compile very efficiently. This code will */
/*        be re-written in assembler when debugged.                         */
/*                                                                          */
/****************************************************************************/
 
void interrupt far irq_16550()
{
    static UINT16   int_status;
                    
    register UINT16 i;                    
                    
    int_status = readIIR(); /* read Interrupt Identifier Register */
        
    switch(int_status & 0x0F)
    {
        case 0:             /* Modem Status Interrupt */
            int_status = readMSR();  /* read modem_status; clears interrupt */
            if(int_status & 1)  /* did CTS change? */
            {
                if (int_status & 0x10) /* is CTS now on?  */
                writeIER(0x0F); /* turn TX interrupts back on. */
            }
            break;
            
        case 1:             /* Priority Interrupt */
            break;          /* priority bit ... unused here... */
            
        case 2:             /* Transmitter Empty Interrupt */
            if(TX_count == 0)   /* If last character... */
            {
               writeIER(0x0D);   /* Mask off TX interrupt... */
               TX_paused_flag = TRUE;
               break;            /* and exit! */
            }
            else
            {
               for(i=0; i<16; i++) /* Put up to 16 characters in the TX FIFO. */
               {
                  writeTHR(*TX_out_pointer);
                  if(++TX_out_pointer >= TX_end_pointer)
                      TX_out_pointer = TX_buffer;
                  --TX_count;

                  if((readMSR() & 0x10) == 0) /* CTS = 0? */
                  {
                    writeIER(0x0D);  /* disable TX interrupts  */
                    break;
                  }
                  if(TX_count == 0) break;
               }
            }
            break;
            
        case 4:             /* RX FIFO Buffer Trigger Interrupt */
        case 0xC:           /* Or... RX Timeout Interrupt.  */
            do
            {
                *RX_in_pointer = readRBR();    /* Get a char to buffer */
                if(++RX_in_pointer >= RX_end_pointer)
                    RX_in_pointer = RX_buffer;
                RX_count++;                     /* increment count */
            } while (readLSR() & 1);            /* repeat until FIFO empty */
            break;
            
        case 6:
        default:          /* All others...including overrun, etc. */
            readLSR();    /* Throw any bogus character on the floor.... */
            readMSR();
       
    }
    int_status = readIER(); /* bang interrupts on and off */
    writeIER (0);
    writeIER(int_status);

    outp(0x20,0x20);        /*  Write General EOI */
}                           /*  ...and dismiss. */

    
/****************************************************************************/
/*                                                                          */
/*  FUNCTION  irq_8250   8250 Interrupt Service Routine                     */
/*                                                                          */
/*  DESCRIPTION                                                             */
/*      Handles interrupts from the 8250 UART.                              */
/*                                                                          */
/*      If the TX buffer becomes empty, the TX interrupt is masked off. It  */
/*      will be re-started by WritePort().                                  */
/*                                                                          */
/*      If the RX buffer overfills, it simply wraps around without warning. */
/*                                                                          */
/****************************************************************************/
 
void interrupt far irq_8250()
{
    static UINT16   int_status;
                    
    int_status = readIIR(); /* read Interrupt Identifier Register */
        
    switch(int_status & 0x07)
    {
        case 0:             /* Modem Status Interrupt */
            readMSR();      /* read modem_status; clears interrupt */
            break;
            
        case 1:             /* Priority Interrupt */
            break;          /* priority bit ... unused here... */
            
        case 2:             /* Transmitter Empty Interrupt */
            if(TX_count == 0)   /* If last character... */
            {
               writeIER(0x0D);   /* Mask off TX interrupt... */
               TX_paused_flag = TRUE;
               break;            /* and exit! */
            }
            else
            {
                writeTHR(*TX_out_pointer);
                if(++TX_out_pointer >= TX_end_pointer)
                    TX_out_pointer = TX_buffer;
                --TX_count;
            }
            break;
            
        case 4:             /* RX FIFO Buffer Trigger Interrupt */
            *RX_in_pointer = readRBR();    /* Get a char to buffer */
            if(++RX_in_pointer >= RX_end_pointer)
                RX_in_pointer = RX_buffer;
            RX_count++;                     /* increment count */
            break;
            
        case 6:
        default:          /* All others...including overrun, etc. */
            readLSR();    /* Throw any bogus character on the floor.... */
            readMSR();
       
    }
    int_status = readIER(); /* bang interrupts on and off */
    writeIER (0);
    writeIER(int_status);

    outp(0x20,0x20);        /*  Write General EOI */
}                           /*  ...and dismiss. */

            
/****************************************************************************/
/*                                                                          */
/* GetPort()    Simply returns the string "COMx" or NULL                    */
/*                                                                          */
/****************************************************************************/
            
char *GetPort(char *arg)
/* return: NULL=failure, else formal DOS name of port */
{
  strcpy(params.port.name, arg);
  return arg;
}


/****************************************************************************/
/*                                                                          */
/* IncomingPort()    Returns the number of characters in the RX buffer.     */
/*                                                                          */
/****************************************************************************/

int     IncomingPort()
{
    return(RX_count);
}

/****************************************************************************/
/*                                                                          */
/*  FUNCTION:                                                               */
/*      ReadPort()    Reads a single character from the serial port.        */
/*                                                                          */
/*  DESCRIPTION:                                                            */
/*      Reads one character from the input buffer if any is present. It     */
/*      first checks the putback buffer to see if there's anything there.   */
/*      If there is, it returns any putback characters first. If there      */
/*      are no characters either in the putback buffer or the RX buffer,    */
/*      the routine waits until one is available,                           */
/*                                                                          */
/*  RETURNS: Returns next character available.                              */
/*                                                                          */
/****************************************************************************/

int ReadPort(int timeout)
{
    int next_char;
    unsigned int time_val, far *dos_clock = (unsigned int far *)0x0000046C;
    
    /* 18 is close enough to 18.2 */
    time_val = (18 * (unsigned)timeout) + *dos_clock; 

    do
    {
	if ((*dos_clock > time_val) && (timeout != -1))
	    return FAIL;
    }
    while(RX_count == 0) ;  /* Wait here until char available */
    
    next_char = (unsigned char)*RX_out_pointer;
    if (++RX_out_pointer >= RX_end_pointer)   
    RX_out_pointer = RX_buffer;
    _asm cli;    /* Guard this bit of code! */
    --RX_count;  /* update counter */
    _asm sti;
    
    return(next_char);
}                                                    
                                               
/****************************************************************************/
/*                                                                          */
/*  FUNCTION:                                                               */
/*      WritePort(char *, count)   Writes "count" characters to serial port.*/
/*                                                                          */
/*  DESCRIPTION:                                                            */
/*      Writes the character string to the serial port buffer, and returns  */
/*      immediately.                                                        */
/*                                                                          */
/*  RETURNS: Returns zero if there is no room in the buffer for the string. */
/*           Otherwise, returns "count" of chars sent to buffer.            */
/*                                                                          */
/****************************************************************************/

int WritePort(UINT8 *string, int n)
{

    int charcount;

    charcount = n;
    if(n + TX_count >= BUFSIZE-32)
        return 0;   /* Sorry, won't fit */
    
    while(charcount--)                      /* otherwise, move the string */
    {
        /* can't use memcpy as buffer might wrap.... */
        *(TX_in_pointer++) = *(string++);
        if(TX_in_pointer >= TX_end_pointer) /* improve this later.... */
            TX_in_pointer = TX_buffer;
        _asm cli;
        TX_count++;
        _asm sti;
    }
    
    if(TX_paused_flag == TRUE)    /* if paused, kick off interrupt */
    {
        writeTHR(*TX_out_pointer);              /* write out one char */
        if(++TX_out_pointer >= TX_end_pointer)
                TX_out_pointer = TX_buffer;
        --TX_count;                             /* decrement count. */
        writeIER(0x0F);                         /* arm the interrupt */
        TX_paused_flag = FALSE;
    }    
    return n;
}    
            
/****************************************************************************/
/*                                                                          */
/*  FUNCTION:                                                               */
/*      ClosePort()    Restores all important values.                       */
/*                                                                          */
/*  DESCRIPTION:                                                            */
/*      Restores interrupt vector and mask, and shuts down the serial port. */
/*      It waits for transmission of any characters in the Xmit buffer.     */
/*                                                                          */
/****************************************************************************/

void ClosePort()
{
    outp(PIC_MASK, PIC_save);   /* Output saved PIC mask */
    
    writeIER( 0 );              /* Disable all UART interrupts */
    
    while( (readLSR() & 0x40) == 0);  /* wait for last char to go out */

    writeFCR( 0 );              /* Shut off FIFO's (if any) */
    
    writeMCR( 0 );              /* Shut off DTR, RTS and Out2 */
 
    _dos_setvect(8 + UART_irq, serial_irq_save);  /* set old interrupt vector */
}
    
    /****** End of PC-COMM.C  ******/                             
