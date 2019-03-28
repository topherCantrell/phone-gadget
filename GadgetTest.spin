CON
        _clkmode        = xtal1 + pll16x
        _xinfreq        = 5_000_000  

{

' ADC/DAC
0-7  Digital sample data
8    DAC -CE
9    ADC -RD 
10     *EXT_SPEAKER
11   ADC -CNV

' SD Card
12   D0
13   CLK
14   DI
15   CS

' LiteLink
16   -OH
17   -CID    
18   -RING
19   LOOP_DET

' TV
20
21
22    

' Keyboard
23   KB-clock
24   KB-data

25   
26   
27   

' ****FUTURE****
' Crossbar to allow for speaker-phone, prompt recording, etc
' Second prop chip?
Phone TX>
Micro   >
DAC     >
Phone RX<
Extern  <
ADC     <
            
---------------------------------

0 SD card
1 LiteLink/XBar
2 ADC/DAC
3 TV  
4 Keyboard
5 Ethernet
6 ?Ethernet?
7    APPLICATION               

}
        
OBJ                       
  adc_dac :  "ADC_DAC_hdw"
  disk    :  "Disk_hdw"
  lite    :  "LiteLinkIII_hdw"
  tone    :  "ToneDetect_hdw"
  
  text    :  "tv_text"
    kb    :  "keyboard"

VAR


  ' For the disk hardware
  long disk_command
  long disk_sectorAddress
  long disk_memoryAddress     

  ' For the ADC/DAC/ToneDetect hardware
  long ToneMode
  long TickCount
  long InputValue
  long ToneDecodeBuffer
  
  long BufferOutStart
  long BufferOutEndP1                                        
  long BufferOutHead
  long BufferOutTail
  long BufferOutStatus
  long BufferOutOp
  long BufferOutLIST
  long BufferOutSIZE
  '    
  long BufferInStart
  long BufferInEndP1                                        
  long BufferInHead
  long BufferInTail
  long BufferInStatus
  long BufferInOp
  long BufferInLIST
  long BufferInSIZE

  ' For the LiteLinkIII
  long HookStatus
  long CIDStatus
  long RingState
  long RingNumber
  long RingOnTime
  long RingOffTime
  long CurrentPulses

  byte decodeBuffer[1024]  
  byte buffer[8192*2]


' c = PlayPrompts(prompts,bargein,timeout)
' Play a list of prompts one after the other. The "bargein" lists the keys that will
' stop the audio and return. Timeout is an optional time to wait after the prompts
' are played.
' The return "c" is either "0-9,*,#,A,B,C,D" for DTMF input (if requested in "bargein") or
' "H" for hangeup or "T" for timeout. The "H" and "T" will always be delivered and do not
' need to be in the "baregin".                                              

' Files are 256 clusters each (1024 sectors, 524288 bytes, 65 seconds).
' There are 100 files. The first 50 are for prompts. The second 50 are for
' recording. Data area is thus 52428800 bytes or 50MBytes.

' The first file of the system is reserved for mailbox information:
' FileInfoStruct (first sector of file)
' N - 00 for old, 01 for new
' R - File number (50..99)
' M - Month of recording (1-12)
' D - Day of recording (1-31)
' W - Day of week of recording (1=Sunday through 7=Saturday)
' Y - Year of recording (2010+Y is actual year)
' CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CID number or "UNKNOWN" (zero terminated)
' NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN - CID name or "UNKNOWN" (zero terminated)
' ... padded to 128 bytes
       
PUB start | key, i, j, k 

' TALK ABOUT DEBUGGING TECHNIQUES. Playing from recording to get consistent
' data to compare to java run. Printing debug block. Counting samples. Endless
' loop for "breakpoint".

    
  ' Start the drive and mount
  disk_command := 1
  disk_memoryAddress := @buffer
  disk.start(3,@disk_command)    
  repeat while disk_command <> 0 

  ' Start the LiteLinkIII driver
  HookStatus   := 0 ' 0=on-hook, 1=off-hook
  CIDStatus    := 0
  RingState    := 0                                             
  RingNumber   := 0
  RingOnTime   := 0
  RingOffTime  := 0                                                     
  lite.start(4,@HookStatus) 
  
  repeat i from 0 to 8181
    buffer[i] := 0  

  ' Start the ADC/DAC driver
  TickCount := 0
  InputValue := 0
  ToneMode := 0
  ToneDecodeBuffer := @decodeBuffer  
  BufferOutStart   := @buffer
  BufferOutEndP1   := @buffer + 8192*2
  BufferOutHead    := @buffer
  BufferOutTail    := @buffer
  BufferOutStatus  := 0
  BufferOutOp      := 0  
  BufferInStart   := @buffer
  BufferInEndP1   := @buffer + 8192*2
  BufferInHead    := @buffer
  BufferInTail    := @buffer 
  BufferInStatus  := 0
  BufferInOp      := 0        
  adc_dac.start(5,@disk_command)

  tone.start(6,@ToneMode)

  ' start video text  
  text.start(20)
   'start the keyboard
  kb.start(23, 24)

  text.str(string("Caller-ID Records",13))
  
   repeat
    RingNumber := 0
    repeat while RingNumber <> 1
    text.str(string("# "))
    waitcnt(clkfreq*1 + cnt)
    CIDStatus := 1
    ToneMode := 2     
    waitcnt(clkfreq*4 + cnt)    
    ToneMode := 3
    CIDStatus := 0
    if(decodeBuffer[0] == 128)
      decodeCID
    else
      text.str(string("ERROR",13))
    waitcnt(clkfreq*20 + cnt)
    text.str(string(" #",13))
         
  repeat     
     key := kb.getKey
     case key
       "y" : text.str(string("RINGS "))
             text.hex(RingNumber,4)
             text.out(13)
       "a" : ToneMode := 2
             text.str(string("CID DETECTING",13))
       "s" : ToneMode := 1
             text.str(string("DTMF DETECTING",13))
       "d" : ToneMode := 3
             text.str(string("TONE STOP",13))
       "o" : text.str(string("ON HOOK",13))   
             HookStatus := 0            
       "h" : text.str(string("OFF HOOK",13))
             HookStatus := 1
             ToneMode := 1      
       "p" : text.str(string("PLAYING SD ... "))
             BufferOutLIST := 0  ' File starts with cluster 0
             BufferOutOp := 2    ' Start file playing
                           
             repeat while (BufferOutOp <> 0)
             text.str(string("...DONE",13))                          
       "r" : text.str(string("RECORDING 10 SECONDS ..."))
             BufferInLIST := 0  ' File starts with cluster 0
             BufferInSIZE := 8000*5
             BufferInOp := 2
             repeat while (BufferInOp <> 0)
             text.str(string("...DONE",13))
       "x" : text.hex(ToneMode,4)
             text.out(13)
             ToneDecodeBuffer := @decodeBuffer
             repeat i from 0 to 80
               j :=  decodeBuffer[i]
               if j==0
                 text.out(".")
               else
                 text.out(j)
             text.out(13)           
       "v" : text.hex(ToneDecodeBuffer,4)
             text.out(" ")
             text.hex(@decodeBuffer,4)
             text.out(13)
             repeat i from 0 to 45               
               text.hex(decodeBuffer[i],2)
               text.out(" ")
             text.out(13)
       "c" : text.hex(CurrentPulses,4)            
             text.out(13)
             CurrentPulses := 0
       "q" : text.str(string("CID ON",13)) 
             CIDStatus := 1
       "w" : text.str(string("CID OFF",13))
             CIDStatus := 0
       "m" : repeat i from $7FD0 to $7FFC STEP 4
               text.hex(long[i],4)
               text.out(" ")
             text.out(13)
       "i" : decodeCID     

PUB decodeCID | i,j,k
             text.hex(decodeBuffer[0],2)
             text.out(" ")
             safeChar(decodeBuffer[4])
             safeChar(decodeBuffer[5])
             text.out("/")
             safeChar(decodeBuffer[6])
             safeChar(decodeBuffer[7])
             text.str(string(" at "))
             safeChar(decodeBuffer[8])
             safeChar(decodeBuffer[9])
             text.out(":")
             safeChar(decodeBuffer[10])
             safeChar(decodeBuffer[11])
             text.out(" ")
             j := decodeBuffer[13]-1             
             repeat i from 0 to j
               safeChar(decodeBuffer[14+i])                       
             k := 14 + decodeBuffer[13]+1
             j := decodeBuffer[k]-1
             k := k+1
             text.out(" ")
             repeat i from 0 to j
               safeChar(decodeBuffer[k+i])             
  
PUB safeChar(s)
  if s<32 or s>127
    text.out("_")
  else
    text.out(s)   