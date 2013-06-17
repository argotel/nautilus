/*   PC-AUDIO.H   */

/* SCCS ID:  @(#)pc-audio.h 1.2 96/03/31 */


#define DMA_BUF_SIZE            2400
                            
   // DMA Programming

#define DMA8_FF_REG      0xC
#define DMA8_MASK_REG    0xA
#define DMA8_MODE_REG    0xB

#define DMA8_READ        0x58
#define DMA8_WRITE       0x54

#define DMA0_ADDR        0
#define DMA0_COUNT       1
#define DMA0_PAGE     0x87
#define DMA1_ADDR        2
#define DMA1_COUNT       3
#define DMA1_PAGE     0x83
#define DMA3_ADDR        6
#define DMA3_COUNT       7
#define DMA3_PAGE     0x82

   // Sound Blaster Defaults

#define DEFAULT_SND_IO               0x220
#define DEFAULT_SND_IRQ              5
#define DEFAULT_SND_DMA              1

   // Sound Blaster DSP Chip 

#define DSP_8BIT_OUTPUT              0x1C
#define DSP_8BIT_INPUT               0x2C
#define DSP_BLOCK_SIZE               0x48
#define DSP_DATA_AVAIL               0xE
#define DSP_PAUSE_DMA                0xD0
#define DSP_RESTART_DMA              0xD4
#define DSP_READ_PORT                0xA
#define DSP_READY                    0xAA
#define DSP_RESET                    0x6
#define DSP_TIME_CONSTANT            0x40
#define DSP_WRITE_PORT               0xC
#define DSP_SPEAKER_ON               0xD1
#define DSP_SPEAKER_OFF              0xD3

   // Sound Blaster Mixer Chip

#define EIGHT_BIT_VOLUME         0x02
#define VOICE_VOLUME             0x04
#define MIC_VOLUME               0x0A
#define MASTER_VOLUME            0x22
#define OUTPUT_GAIN              0x41
#define INPUT_GAIN               0x3F

#define MIXER_ADDR               0x04
#define MIXER_DATA               0x05

#define	VOICE_LEVEL_LOW		 0xCC		/* 4 bits per channel */
#define VOICE_LEVEL_MEDIUM	 0xDD
#define VOICE_LEVEL_HIGH	 0xEE
#define MASTER_LEVEL_LOW	 0xDD		/* 4 bits per channel */
#define MASTER_LEVEL_MEDIUM	 0xEE
#define MASTER_LEVEL_HIGH	 0xFF
#define	MIC_LEVEL_LOW		 0x04		/* "low" mic sensitivity */
#define MIC_LEVEL_MEDIUM	 0x05		/* "medium" (default)    */
#define MIC_LEVEL_HIGH		 0x06		/* "high" mic sensitivity */
#define DEFAULT_INPUT_GAIN       0xC0		/* 4 bits in high nibble? */
#define DEFAULT_OUTPUT_GAIN      0x80		/* 4 bits in high nibble? */


#define PIC_END_OF_INT           0x20
#define PIC_MASK                 0x21
#define PIC_MODE                 0x20

/******************************************************************/
/*---------  FUNCTION PROTOTYPES  --------------------------------*/
/******************************************************************/

INT8      GetBlasterEnv(void),
          InitDMA(UINT32, INT16),
          ResetDSP(INT16);
            
void      DSPOut(INT16, INT16),
          SetMixer(INT16, INT16, INT16),
          CloseAudio(void),
          sleep(UINT16);

void interrupt far DMA_Isr(void);   /* Interrupt Service Routine */

void    (interrupt far *gIRQSave)();  /* Save old IRQ vector decl. */

/******************************************************************/
/*---------  GLOBAL DECLARATIONS  --------------------------------*/
/******************************************************************/

UINT8   gDMABuffer[DMA_BUF_SIZE],
        *gIOPointer,
        gByteTimeConstant;
         
INT16   gIOPort,
        gFrameSize,
        gEndOfFrame, 
        volatile gFrameCount,
        gDMAPaused,
        gDMARunning,
        gReadingFlag,

        gMicVol,
        gVoiceVol,
        gMasterVol,
        g8BitVol,
        gInputGainLeft,
        gInputGainRight,
        gOutputGainLeft,
        gOutputGainRight,

        gMaxFrames,
        gBufferSize, 
        gDMAChan,
        gIRQNumber,
        gIRQMask,
        gMaskSave;

   
UINT32  gBufPhysAddr;
