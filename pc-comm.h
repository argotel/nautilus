/*  "pc-comm.h"

    Header file for the 16550/8250 UART in the IBM PC environment.

    Original code by:

    National Semiconductor Corporation
    Microcomputer Systems Group
    Written by : Louis Shay     1/11/89

    Adapted for the Nautilus program:

    Patrick Mullarky
    NW Computer Engineering  9/26/94

Note: The following definitions are used in programmong both the
NS16550-family and the NS8250-family of UART chips.

SCCS ID:  @(#)pc-comm.h 1.1 96/02/29
*/

/* define default I/O port addresses */

#define COM_1  0x03f8
#define COM_2  0x02f8
#define COM_3  0x03e8
#define COM_4  0x02e8

#define COM1_IRQ  4
#define COM2_IRQ  3
#define COM3_IRQ  4
#define COM4_IRQ  3

/* UART register definitions using UART base address, int UART_addr */



#define RBR (UART_addr+0) /* Receive Buffer Register      (R  )(DLAB==0) */
#define THR (UART_addr+0) /* Transmitter Holding Register (  W)(DLAB==0) */
#define IER (UART_addr+1) /* Interrupt Enable Register    (R/W)(DLAB==0) */
#define IIR (UART_addr+2) /* Interrupt Ident. Register    (R  )          */
#define FCR (UART_addr+2) /* FIFO Control Register        (  W) (16550 only)  */
#define LCR (UART_addr+3) /* Line Control Register        (R/W)          */
#define MCR (UART_addr+4) /* MODEM Control Register       (R/W)          */
#define LSR (UART_addr+5) /* Line Status Register         (R  )          */
#define MSR (UART_addr+6) /* MODEM Status Register        (R/W)          */
#define SCR (UART_addr+7) /* SCratch Register             (R/W)          */
#define DLL (UART_addr+0) /* Divisor Latch (LSB)          (R/W)(DLAB==1) */
#define DLM (UART_addr+1) /* Divisor Latch (MSB)          (R/W)(DLAB==1) */
#define AFR (UART_addr+2) /* Alternate Function Register  (R/W) */

/* register read/write macros */

#define readRBR()    ((int)inp(RBR))    /* read RBR */
#define readDLL()    ((int)inp(DLL))    /* read DLL */
#define readDLM()    ((int)inp(DLM))    /* read DLM */
#define readIER()    ((int)inp(IER))    /* read IER */
#define readIIR()    ((int)inp(IIR))    /* read IIR */
#define readLCR()    ((int)inp(LCR))    /* read LCR */
#define readMCR()    ((int)inp(MCR))    /* read MCR */
#define readLSR()    ((int)inp(LSR))    /* read LSR */
#define readMSR()    ((int)inp(MSR))    /* read MSR */
#define readSCR()    ((int)inp(SCR))    /* read SCR */
#define readAFR()    ((int)inp(AFR))    /* read AFR */

#define writeTHR(val) (outp(THR, val))   /* write THR */
#define writeDLL(val) (outp(DLL, val))   /* write DLL */
#define writeDLM(val) (outp(DLM, val))   /* write DLM */
#define writeIER(val) (outp(IER, val))   /* write IER */
#define writeFCR(val) (outp(FCR, val))   /* write FCR (16550 only) */
#define writeLCR(val) (outp(LCR, val))   /* write LCR */
#define writeMCR(val) (outp(MCR, val))   /* write MCR */
#define writeLSR(val) (outp(LSR, val))   /* write LSR */
#define writeMSR(val) (outp(MSR, val))   /* write MSR */
#define writeSCR(val) (outp(SCR, val))   /* write SCR */
#define writeAFR(val) (outp(AFR, val))   /* write AFR */

/* Removed references to device "2".   plm */

#define BUFSIZE         1000
#define PUTBACK_SIZE    10
    
#define PIC_MASK        0x21

        
        /* PROTOTYPES */
        
UINT16  port_id(void);
void    ClosePort(void);

void    far interrupt irq_8250();        
void    far interrupt irq_16550();        
        
/* end of pc-comm.h */
