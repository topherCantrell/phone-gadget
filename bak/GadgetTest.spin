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

23   CS
24   SCK
25   SI
26   SO
27   INT

' Serial EPROM
28   SCL
29   SDA

' USB (programming) and Keyboard (after boot)
30   RX 
31   TX

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
  deb     :  "TVDebug_client"
  adc_dac :  "ADC_DAC_hdw"
  disk    :  "Disk_hdw"
  lite    :  "LiteLinkIII_hdw"
  tone    :  "ToneDetect_hdw"   

VAR


  ' For the disk hardware
  long disk_command
  long disk_sectorAddress
  long disk_memoryAddress     

  ' For the ADC/DAC/ToneDetect hardware
  long Mode
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
       
PUB start | key, i, j


  'repeat
    '
    
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

  tone.start(6,@TickCount)
  
  deb.c_start(24)
  deb.c_reset
  deb.c_cls      
  
  {
  RingNumber := 0  
  repeat
    deb.c_string(string("STARTED ",13))    
    repeat while RingNumber <> 2
    waitcnt(clkfreq + cnt) 
    HookStatus := 1
    BufferOutLIST := 0  ' File starts with cluster 0
    waitcnt(clkfreq/10 + cnt) 
    BufferOutOp := 2    ' Start file playing
    repeat while (BufferOutOp <> 0)
    HookStatus := 0
    waitcnt(clkfreq+cnt)
    RingNumber := 0

  ' o = on-hook
  ' h = off-hook        
  ' r = record to ram
  ' p = playback from ram
  ' s = save ram to SD
  ' l = load ram from SD
  ' d = display buffer
  ' c = show loop-current counters
  }
  
  repeat
     key := 0
     repeat while key == 0
       waitcnt(clkfreq/10 + cnt)
       key := deb.c_getKey
     case key
       "o" : deb.c_string(string("ON HOOK",13))   
             HookStatus := 0            
       "h" : deb.c_string(string("OFF HOOK",13))
             HookStatus := 1            
       "p" : deb.c_string(string("PLAYING SD ... "))
             BufferOutLIST := 0  ' File starts with cluster 0
             BufferOutOp := 2    ' Start file playing
             repeat while (BufferOutOp <> 0)
             deb.c_string(string("...DONE",13))
             'deb.c_long(long[$7FF0])
             'deb.c_char(13)               
       "r" : deb.c_string(string("RECORDING 10 SECONDS ..."))
             BufferInLIST := 0  ' File starts with cluster 0
             BufferInSIZE := 8000*5
             BufferInOp := 2
             repeat while (BufferInOp <> 0)
             deb.c_string(string("...DONE",13))
       "x" : ToneDecodeBuffer := @decodeBuffer
             repeat i from 0 to 20
               j :=  decodeBuffer[i]
               decodeBuffer[i]:=0
               if j==0
                 deb.c_char(".")
               else
                 deb.c_char(j)
             deb.c_char(13)    
       "y" : waitcnt(clkfreq*5 + cnt) 
             {
       "p" : deb.c_string(string("PLAYING..."))
             BufferOutHead    := @buffer
             BufferOutTail    := @buffer
             BufferOutStatus  := 0
             BufferOutOp      := 5 
             repeat while (BufferOutOp <> 0)
             deb.c_string(string("...DONE",13))                       
       "r" : deb.c_string(string("RECORDING..."))
             BufferInHead     := @buffer
             BufferInTail     := @buffer 
             BufferInStatus   := 0
             BufferInOp       := 5
             repeat while (BufferInOp <> 0)
             deb.c_string(string("...DONE",13))               
       "s" : deb.c_string(string("SAVING..."))
             disk_memoryAddress := @buffer
             disk_sectorAddress := 0
             disk_command := %10_00100000  ' Write 8*2 = 16K
             repeat while disk_command <> 0
             deb.c_string(string("...DONE",13))
       "l" : deb.c_string(string("LOADING..."))
             disk_memoryAddress := @buffer
             disk_sectorAddress := 0
             disk_command := %01_00100000  ' Read 8*2 = 16K
             repeat while disk_command <> 0
             deb.c_string(string("...DONE",13))
             }
       "d" : deb.c_char(13)
             repeat i from 0 to 9               
               repeat j from 0 to 12
                 deb.c_byte(buffer[i*13+j])
                 deb.c_char(" ")
               deb.c_char(13)
       "c" : deb.c_long(CurrentPulses)            
             deb.c_char(13)
             CurrentPulses := 0
       "q" : deb.c_string(string("CID ON",13)) 
             CIDStatus := 1
       "w" : deb.c_string(string("CID OFF",13))
             CIDStatus := 0 
             
             
        
  {  
  repeat
    x := deb.c_getKey
    deb.c_byte(x)
    deb.c_char(" ")
    waitcnt(clkfreq + cnt) 

  repeat

  ' Build a pattern in the output buffer
  repeat x from 0 to 16
    outputBuffer[x] := x*5+$40
  BufferOutStart   := @outputBuffer
  BufferOutEndP1   := @outputBuffer + 2048
  BufferOutHead    := @outputBuffer
  BufferOutTail    := @outputBuffer + 16
  BufferOutStatus  := 0
  BufferOutOp      := 1

  ' Clear the input buffer
  repeat x from 0 to 2047
    inputBuffer[x] := $AA ' A non-zero value
  BufferInStart   := @inputBuffer
  BufferInEndP1   := @inputBuffer + 2048
  BufferInHead    := @inputBuffer
  BufferInTail    := @inputBuffer + 16
  BufferInStatus  := 0
  BufferInOp      := 1

  ' Let the driver do the magic
  adc_dac.start(4,@BufferOutStart)

  waitcnt(clkfreq + cnt)
  waitcnt(clkfreq + cnt)
  waitcnt(clkfreq + cnt)
     
  deb.c_start(24)
  deb.c_reset
  deb.c_cls

  repeat x from 0 to $3F
    deb.c_byte(inputBuffer[x])
    deb.c_char(" ")

  deb.c_char(13)
  deb.c_char(13)
  deb.c_byte(BufferOutStatus)
  deb.c_char(13)
  deb.c_byte(BufferOutOp)
  deb.c_char(13)
  deb.c_byte(byte[$7FFF])

  deb.c_char(13)
  deb.c_char(13)
  deb.c_byte(BufferInStatus)
  deb.c_char(13)
  deb.c_byte(BufferInOp)
   
  repeat
       
    
  
  y:=cnt
  y := y-x

  deb.c_long(y)
  deb.c_char(13)
  
  repeat a from 0 to 20
    deb.c_byte(buffer[a])
    deb.c_char(" ")

  deb.c_char(13)

  x:=cnt
  disk_memoryAddress := @buffer
  disk_sectorAddress := 0
  disk_command := %10_00000100
  repeat while disk_command <> 0
  y:=cnt
  y := y-x

  deb.c_long(y)
  deb.c_char(13)
  deb.c_long(clkfreq)    

  repeat
  }
  
    