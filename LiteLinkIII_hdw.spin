CON

' The RING pin will alternate when ringing 20Hz in US and 25Hz in Europe
' In US the cadence is Ring:2000ms,Pause:4000ms,Repeat
' In Eurpode the cadence is Ring:400ms,Pause:200ms,Ring:400ms,Pause:2000ms,Repeat
' CID is sent during the pause after the 1st ring burst

' The hardware here runs on a 10Hz clock. Ringing is collected over each 10Hz interval.
' If the interval has a single ring-detect the interval is marked as a ring interval.
'
' The hardware bumps the counts on "RingOn" and "RingOff" and on "RingNumber".
' The hardware keeps ringing state in "RingState".
' In the US it would look like this:
' Start with RingNumber, RingOn, and RingOff set to 0
' "RingState" will be 0 since there is no ringing
' When first ring is detected "RingState" will go to 1 and "RingNumber" immediately
' goes to 1. "RingOn" begins counting to 20 (2 seconds). When the first ring ends the
' "RingState" will go to 0 and "RingOff" will begin counting to 20 (2 seconds).
' On the second ring "RingState" will go to 2, "RingNumber" immediately goes to 2,
' and "RingOn" continues counting up from 20 (from before) to 40.
'
' The 911DET line seems to stay low until there is loop current activity. Then it pulses
' high. It wiggles a few times when the LITELINKIII goes on or off hook. It wiggles a
' lot when the phone is ringing. It wiggles a little when the remote side disconnects
' (which is what we are interested in here).
'
' When a call is connected and another comes in the loop current does NOT wiggle. Thus
' any wiggles after a call is in progress may be interpreted as a disconnect.

PUB start(cog,param)
  
  coginit(cog,@LiteLinkDriver,param)

DAT      
         org 0

LiteLinkDriver

         mov       outa,C_PIN_MASK
         mov       dira,C_PIN_MASK
         
         mov       frqa,#1             ' Incrememnt by one each edge  
         mov       ctra, C_CTRA        ' Count edges of RING line
         mov       frqb,#1             ' Increment by one each edge
         mov       ctrb, C_CTRB        ' Count edges of 911DET line   

         ' Copy parameter block
         mov       tmp,par
         mov       value,#HookStatus
         mov       tmp2,#7   
sam_init movd      smi_ptr,value
         add       value,#1
smi_ptr  mov       0,tmp
         add       tmp,#4
         djnz      tmp2,#sam_init
                
         ' Init the timer for the first wait 
         mov       timer,cnt
         add       timer,C_RATE
                  
main

         ' Copy the hook status to the hook pin (0 means ON-HOOK-1, 1 means OFF-HOOK-0)
         ' Copy the cid status to the hook pin  (0 means OFF-1, 1 means ON=0)
         mov       tmp,outa
         rdlong    tmp2,HookStatus wz
  if_z   or        tmp,C_MASK_o
  if_nz  andn      tmp,C_MASK_o  
         rdlong    tmp2,CIDStatus wz
  if_z   or        tmp,C_MASK_c
  if_nz  andn      tmp,C_MASK_c  
         mov       outa,tmp           

         ' Reset count of RING pulses               
         mov       phsa,#0
         ' Reset count of 911DET pulses
         mov       phsb,#0            

         ' Wait for next 10th of a second                     
         waitcnt   timer,C_RATE                   

         ' Count pulses on the 911DET
         rdlong    tmp,CurrentPulses
         mov       tmp2,phsb
         add       tmp,tmp2
         wrlong    tmp,CurrentPulses             

         ' Read current state
         mov       oldRState,ringst    ' Remember what we were before
         mov       ringst,phsa wz      ' New state        

         ' The hardware seems to really mean NO-RING over an interval, but
         ' has lots of false RING-DET spikes sprinkled through. This
         ' algorithm makes sure we get 4 intervals of RING-DET before actually
         ' recognizing a ring.
  if_z   mov       inarow,#0
  if_z   jmp       #ring2
         cmp       inarow,#4 wz
  if_z   mov       ringst,#1
  if_z   jmp       #ring2
         add       inarow,#1
         mov       ringst,#0
ring2                      

         cmp       ringst,#0 wz
  if_z   jmp       #bumpRingOff

         cmp       oldRState,#0 wz
  if_nz  jmp       #alreadyOn

         ' Going from off to on ... count as a ring
         rdlong    tmp,RingNumber
         add       tmp,#1
         wrlong    tmp,RingNumber
         
alreadyOn
         ' RingState is on ... count OnTime
         rdlong    tmp,RingOnTime         
         add       tmp,#1
         wrlong    tmp,RingOnTime
         jmp       #main

bumpRingOff
        ' RingState is off ... count OffTime
         rdlong    tmp,RingOffTime
         add       tmp,#1
         wrlong    tmp,RingOffTime  
         jmp       #main


tmp            long  0
tmp2           long  0
value          long  0
timer          long  0
ringst         long  0
oldRState      long  0
inarow         long  0

                               'LRCO_dcis_VBRD_dddddddd                        
C_PIN_MASK     long   %00000000_0011_0000_0000_00000000
C_MASK_o       long   %00000000_0001_0000_0000_00000000
C_MASK_c       long   %00000000_0010_0000_0000_00000000
C_MASK_r       long   %00000000_0100_0000_0000_00000000
C_MASK_l       long   %00000000_1000_0000_0000_00000000
C_CTRA         long   %01100 << 26 + 18     ' Mode + APIN (18)
C_CTRB         long   %01010 << 26 + 19     ' Mode + APIN (19) 

' 80_000_000 clocks  / second
'         10 samples / second
'
'   8_000_000 clocks / sample
C_RATE         long 80_000_000/10
C_B_HIGH       long $0000_FFFF
C_B_LOW        long $0000_00FF    

HookStatus     long  0
CIDStatus      long  0
'
RingState      long  0
RingNumber     long  0
RingOnTime     long  0
RingOffTime    long  0
CurrentPulses  long  0  

         