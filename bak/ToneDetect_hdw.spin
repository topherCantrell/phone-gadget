CON

' Mode 
' TickCount    (increments with each new sample)
' InputValue   (the last sample received)
' DecodeBuffer (pointer to 128 bytes)

' Mode:
'  0 = stopped
'  1 = DTMF
'  2 = CID
'  3 = StopRequest  


PUB start(cog,param)
  
  coginit(cog,@ToneDetectDriver,param)

DAT      
         org 0

ToneDetectDriver

        ' Copy parameter block
         mov       tmp,par             ' Source
         mov       ptr,#Mode           ' Destination
         mov       tmp2,#4             ' 4 to copy
sam_init movd      smi_ptr,ptr         ' Set destination
         add       ptr,#1              ' Bump pointer
smi_ptr  mov       0,tmp               ' Save address
         add       tmp,#4              ' Next long in shared memory
         djnz      tmp2,#sam_init      ' Do all in parameters

         rdlong    lastCnt,tickCount   ' Starting sample tick count
   
main
         rdlong    tmp,tickCount       ' Get the current tick count
         cmp       tmp,lastCnt wz      ' Has it changed?
    if_z jmp       #main               ' No ... wait for a new sample
         mov       lastCnt,tmp         ' This is where we are in the count

         rdlong    sampleValue,inputValue   ' Get the input value
         
         call      #sampleIn           ' Roll it through the Goertzel

         djnz      samplesToGo,#main   ' All 205 (window size) done? No ... go back
         mov       samplesToGo,#205    ' Reload 205 counter

         call      #calcMag            ' Calculate the Goertzel magnitudes on the past window

         call      #processButton      ' See if there is a DTMF key in the mags         

         jmp       #main               ' Back to top

sampleIn        
         mov       ptr3,#COEFF_0       ' First freqency block
         mov       count,#7            ' Seven frequencies to do         
siloop   mov       ptr,ptr3            ' Source is current block
         mov       ptr2,#COEFF_n       ' Destination is working vars
         call      #copy5              ' Copy current frequency to working area
         call      #goertzel_sample    ' Add the sample to the working frequency
         mov       ptr,#COEFF_n        ' Source is working vars
         mov       ptr2,ptr3           ' Destination is current block
         call      #copy5              ' Copy calculated vars back 
         add       ptr3,#5             ' Next frequency block
         djnz      count,#siloop       ' Do all 7 blocks  
sampleIn_ret                           '
         ret                           ' Done

calcMag
         mov       ptr3,#COEFF_0       ' First frequency block
         mov       count,#7            ' Seven freqencies to do
cmloop   mov       ptr,ptr3            ' Source is current block
         mov       ptr2,#COEFF_n       ' Destination is working vars
         call      #copy5              ' Copy current frequency to working area
         call      #goertzel_mag       ' Calculate the magnitude for working frequency
         mov       ptr,#COEFF_n        ' Source is working vars
         mov       ptr2,ptr3           ' Destination is current block
         call      #copy5              ' Copy calculated vars back
         add       ptr3,#5             ' Next frequency block
         djnz      count,#cmloop       ' Do all 7 blocks
calcMag_ret                            '
        ret                            ' Done
                 

goertzel_sample
{
                for(int i=0; i<7; i++) {
                        if ((Q0[i]&0x80000000)!=0) { 
                                // if Q0 is negative                    
                                Q0[i] = ~((COEFF[i]*~Q1[i])>>12) - Q2[i] + value;
                        } else {
                                // else Q0 is positive
                                Q0[i] =  ((COEFF[i]*Q1[i])>>12) - Q2[i] + value;
                        }
                        Q2[i] = Q1[i];
                        Q1[i] = Q0[i];
                }
}         
         

         mov       t1,COEFF_n
         mov       t2,Q1_n
         and       Q0_n,C_80000000 wz, nr
  if_z   jmp       #si_0a
         xor       t2,C_FFFFFFFF
         call      #multiply            
         shr       t1,#12
         xor       t1,C_FFFFFFFF             
         jmp       #si_0b
si_0a    call      #multiply
         shr       t1,#12
si_0b    sub       t1,Q2_n
         add       t1,sampleValue
         mov       Q0_n,t1
         mov       Q2_n,Q1_n
         mov       Q1_n,Q0_n    
         
goertzel_sample_ret
         ret
         


goertzel_mag
{
                for (int i=0; i<7; i++) {
                        int sign = 0;
                        long a = Q1[i];                                                         
                        if ((a&0x80000000)!=0) {
                                a = ~a; 
                                sign+=1;                                
                        }

                        long b = Q2[i];                                                         
                        if ((b&0x80000000)!=0) {
                                b = ~b;
                                sign+=1;
                                
                        }

                        long c = a*b;
                        c = (c*COEFF[i]) >> 12;
                        b = b*b;
                        a = a*a;
                        if ((sign&1)!=0) {
                                mag[i] = a + b + c;
                        } else {
                                mag[i] = a + b - c;
                        }
                        Q0[i] = 0;
                        Q1[i] = 0;
                        Q2[i] = 0;                      
                }       
}
           
         mov       sign,#0
         mov       a,Q1_n
         and       a,C_80000000 wz, nr
  if_z   jmp       #gm1
         xor       a,C_FFFFFFFF
         add       sign,#1
gm1      mov       b,Q2_n
         and       b,C_80000000 wz, nr
  if_z   jmp       #gm2
         xor       b,C_FFFFFFFF
         add       sign,#1
gm2      mov       t1,a
         mov       t2,b
         call      #multiply
         mov       t2,COEFF_n
         call      #multiply
         shr       t1,#12
         mov       c,t1        
         mov       t1,b
         mov       t2,b
         call      #multiply
         mov       b,t1
         mov       t1,a
         mov       t2,a
         call      #multiply
         mov       a,t1
         and       sign,#1 wz
  if_z   jmp       #gm3
         add       a,b
         add       a,c           
         jmp       #gm4
gm3      add       a,b
         sub       a,c
gm4      mov       MAG_n,a
         mov       Q0_n,#0
         mov       Q1_n,#0
         mov       Q2_n,#0
                
goertzel_mag_ret
         ret


processButton

         ' Find largest row
         mov       rowNum,#0                    '
         mov       rowVal,MAG_0                 '
         cmp       rowVal,MAG_1 wz, wc          '
  if_be  mov       rowVal,MAG_1                 '
  if_be  mov       rowNum,#1                    '
         cmp       rowVal,MAG_2 wz, wc          '
  if_be  mov       rowVal,MAG_2                 '
  if_be  mov       rowNum,#2                    '
         cmp       rowVal,MAG_3 wz, wc          '
  if_be  mov       rowVal,MAG_3                 '
  if_be  mov       rowNum,#3                    '

         ' Find largest column
         mov       colNum,#4                     '
         mov       colVal,MAG_4                  '
         cmp       colVal,MAG_5 wz, wc           '
  if_be  mov       colVal,MAG_5                  '
  if_be  mov       colNum,#5                     '
         cmp       colVal,MAG_6 wz, wc           '
  if_be  mov       colVal,MAG_6                  '
  if_be  mov       colNum,#6                     '
    
         ' Make sure both are loud enough       
         cmp       colVal,THRESHOLD wz, wc  ' Column tone loud enough?
  if_be  mov       lastChar,#0              ' No ... reset last-char and ...
  if_be  jmp       #processButton_ret       ' ... skip
         cmp       rowVal,THRESHOLD wz, wc  ' Row tone loud enough?
  if_be  mov       lastChar,#0              ' No ... reset last-char and ...
  if_be  jmp       #processButton_ret       ' ... skip
                                                   
         sub       colNum,#4  
         mov       tmp,rowNum          ' tmp = rowNum *1
         add       tmp,rowNum          ' tmp = rowNum *2
         add       tmp,rowNum          ' tmp = rowNum *3
         add       tmp,colNum          ' tmp = rowNum*3+colNum            
         add       tmp,#MAP            ' Offset in digit table 

         movs      pb1,tmp             ' Write pointer
         nop                           '
pb1      mov       tmp,0               ' Get button value to tmp
         
         cmp       tmp,lastChar wz,wc      ' Is this the same as before
  if_z   jmp       #pb2                    ' Yes ... go count it
         mov       lastChar,tmp            ' Remember this for next time
         mov       lastCharCnt,#1          ' Reset count on new value
         mov       tmp,#0                  ' Ignore this
         jmp       #processButton_ret      ' Save and done

pb2      add       lastCharCnt,#1          ' Increment count on this value
         cmp       lastCharCnt,#5 wz, wc   ' We record on exactly 5 (no more, no less)
  if_ne  jmp       #processButton_ret
  
         rdlong    ptr,decodeBuffer    ' Get address of decode buffer        
         wrbyte    tmp,ptr             ' Put button in buffer

         add       ptr,#1              ' Bump input ...
         wrlong    ptr,decodeBuffer    ' ... buffer cursor
         
processButton_ret                      '
         ret                           ' Done

' -------------------------------------------------------------------------------------
' Copy 5 internals from ptr->ptr2
copy5    mov       cnt2,#5             ' 5 locations to move
copy5_m  movs      sia,ptr             ' Source from ptr
         movd      sia,ptr2            ' Dest from ptr2
         add       ptr,#1              ' Advance ...
         add       ptr2,#1             ' ... pointers
sia      mov       0,0                 ' Do the copy
         djnz      cnt2,#copy5_m       ' Do all 5
copy5_ret                              '
         ret                           ' Out

' -------------------------------------------------------------------------------------
' Multiply
'
'   in:  t1 = 16-bit multiplicand
'        t2 = 16-bit multiplier
'   out: t1 = 32-bit product
'
multiply
         and       t1,C_FFFF
         and       t2,C_FFFF
         mov       t3,#16
         shl       t2,#16
         shr       t1,#1 wc
mloop
  if_c   add       t1,t2 wc
         rcr       t1,#1 wc
         djnz      t3,#mloop
multiply_ret
         ret

t1  long 0
t2  long 0
t3  long 0        
       
THRESHOLD long $20000

C_FFFFFFFF long $FF_FF_FF_FF
C_80000000 long $80_00_00_00
C_FFFF     long $FF_FF

tmp            long  0
tmp2           long  0
ptr            long  0
ptr2           long  0
ptr3           long  0
count          long  0
lastCnt        long  0
cnt2           long  0
sign           long  0
a              long  0
b              long  0
c              long  0
sampleValue    long  0

lastChar       long  0
lastCharCnt    long  0

rowNum         long  0
rowVal         long  0
colNum         long  0
colVal         long  0

samplesToGo    long  205

' Parameter block
mode           long 0
tickCount      long 0
inputValue     long 0  
decodeBuffer   long 0

bufptr         long 0


MAP long "1","2","3",   "4","5","6",    "7","8","9",    "*","0","#"   

' Working variables
COEFF_n long  0
Q0_n    long  0
Q1_n    long  0
Q2_n    long  0
MAG_n   long  0

' 7 sets of 5 variables ... one set per frequency
COEFF_0 long  $1B52
Q0_0    long  0
Q1_0    long  0
Q2_0    long  0
MAG_0   long  0

COEFF_1 long  $1A53
Q0_1    long  0
Q1_1    long  0
Q2_1    long  0
MAG_1   long  0

COEFF_2 long  $1919
Q0_2    long  0
Q1_2    long  0
Q2_2    long  0
MAG_2   long  0

COEFF_3 long  $17A6
Q0_3    long  0
Q1_3    long  0
Q2_3    long  0
MAG_3   long  0

COEFF_4 long  $12A0
Q0_4    long  0
Q1_4    long  0
Q2_4    long  0
MAG_4   long  0

COEFF_5 long  $0FF1
Q0_5    long  0
Q1_5    long  0
Q2_5    long  0
MAG_5   long  0

COEFF_6 long  $0CC7
Q0_6    long  0
Q1_6    long  0
Q2_6    long  0
MAG_6   long  0


  


