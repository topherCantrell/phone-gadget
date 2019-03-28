CON

' 8_000 samples/sec
' Clock rate is 80_000_000Hz /4
'               20_000_000Hz / 8_000Hz =
'                    2_500 instructions between samples

' I/O  VBRD_dddddddd

' BufferOut... then BufferIn...
'
' Buffer**Start    Address of beginning of buffer (bytes)
' Buffer**EndP1    End of buffer plus one                                    
' Buffer**Head     Pointer to next byte for hardware
' Buffer**Tail     Pointer to next available byte
' Buffer**Status   Incremented if hardware catches the tail
' Buffer**Op       0 means halted
'                  1 means halt request 
'                  2 means file-mode request (read/write sectors)                   
'
' Buffer**LIST    Pointer to list of clusters to fill with input samples
' Buffer**SIZE    Total number of bytes to written to file (or bytes to play)

' Hardware advances head. Software advances tail. Software can never advance
' tail to equal head. Hardware can never advance head to equal tail.

' In file-mode the hardware reads/writes a list of sectors pointed to by
' Buffer*LIST. The hardware will advance this pointer in place stopping
' when the value is 0. For the sample-player, the hardware will count down
' the BufferOutSize until all are played and then halt. For the sample-reader
' the hardware will increment BufferInSIZE until stopped.  

  PIN_DAC     =  8   ' output
  PIN_RD      =  9   ' output                                                
  PIN_BSY     = 10   ' input
  PIN_CONVERT = 11   ' output


PUB start(cog,param)
  
  coginit(cog,@SampleDriver,param)

DAT      
         org 0

SampleDriver             
         
         ' Configure I/O
         mov       outa,C_PIN_MASK         ' All signals high (de-assert)
         mov       dira,C_PIN_MASK         ' Control pin directions and bus as input

        ' Copy parameter block
         mov       tmp,par
         mov       ptr,#d_command
         mov       tmp2,#23   
sam_init movd      smi_ptr,ptr
         add       ptr,#1
smi_ptr  mov       0,tmp
         add       tmp,#4
         djnz      tmp2,#sam_init

        ' Init the timer for the first wait 
         mov        timer,cnt
         add        timer,C_RATE_D          
        
main     ' Wait for the next 10_000 tick (8K/sec)
         ' or 9600Hz if in CallerID mode
         rdlong    tmp,Mode
         cmp       tmp,#2 wz     
  if_z   waitcnt   timer,C_RATE_C
  if_nz  waitcnt   timer,C_RATE_D                         
                                                                   
         call      #adc_start          ' Trigger the conversion 
         call      #dac_out            ' Write dacValue to hardware
         call      #adc_wait           ' Wait until conversion is done
         call      #adc_in             ' Read adcValue from hardware

          ' Write the raw sample values for any "raw" consumer like a tone detector.
         mov       tmp,adcValue        ' Write ...
         call      #toneOut            ' ... adcValue (detect on output for debug)          

doDAC    jmp       #doDAC_IDLE         ' SOURCE filed filled in with current state
doDAC_DONE

doADC    jmp       #doADC_IDLE         ' SOURCE filed filled in with current state
doADC_DONE

         jmp       #main               ' Top of loop

toneOut  wrlong    tmp,InputValue      ' Write the new value       
         rdlong    tmp,TickCount       ' Tell ...
         add       tmp,#1              ' ... consumer about ...
         wrlong    tmp,TickCount       ' ... new value
toneOut_ret                            '
         ret                           ' Done

doDAC_IDLE
         rdlong    tmp,BufferOutOp     ' Get request
         cmp       tmp,#2 wz           ' Is it a file request?
  if_z   jmp       #doDAC_startFile    ' Yes ... go start the file
doD_hlt  mov       tmp,#0              ' All other ...
         wrlong    tmp,BufferOutOp     ' ... requests are cleared
         movs      doDAC,#doDAC_IDLE   ' Return to IDLE state    
         jmp       #doDAC_DONE         ' Move on
         
doDAC_startFile
         ' Load the 1st cluster (includes the WAV header)
         rdlong    tmp,BufferOutStart      ' Start of buffer
         wrlong    tmp,BufferOutHead       ' Set head to start of buffer
         wrlong    tmp,BufferOutTail       ' Tail at head ... buffer empty
         call      #readToTail             ' Start loading the 1st cluster

         ' Wait for the 1st cluster to load
doDACw1  rdlong    tmp,d_command wz        ' ## Keep ticking ...
  if_nz  movs      doDAC,#doDACw1          ' ## ... back until ...
  if_nz  jmp       #doDAC_DONE             ' ## ... cluster loads

         ' Start caching next cluster
         call      #readToTail             ' Start caching next cluster

         ' Get the length from the WAV header
         rdlong    ptr,BufferOutHead       ' Read ...
         add       ptr,#40                 ' ... data ...
         rdlong    tmp,ptr                 ' ... length from ...
         add       ptr,#4                  ' ... wav header
         wrlong    ptr,BufferOutHead       ' Head points to data
         wrlong    tmp,BufferOutSIZE       ' Keep up with bytes left to play   
         mov       outCount,C_2048         ' 2K bytes till this cluster is done
         sub       outCount,#44            ' Minus the 44 byte header we skipped

         ' Play loop. Check for abort.
doDAC_a  rdlong    tmp,BufferOutOp         ' Get any request
         cmp       tmp,#1 wz               ' Stop requested?
 if_z    jmp       #doD_hlt                ' Yes ... we can just stop
         mov       tmp,#2                  ' All other requests ...
         wrlong    tmp,BufferOutOp         ' ... reset back to file-play

         ' Check for EOF
         rdlong    tmp,BufferOutSIZE wz    ' Bytes left to write
 if_z    jmp       #doD_hlt                ' All done ... go halt
         sub       tmp,#1                  ' Decrement ...
         wrlong    tmp,BufferOutSIZE       ' ... the count

         ' Read next sample and bump the head
         rdlong    ptr,BufferOutHead       ' Read next ...
         rdbyte    dacValue,ptr            ' ... sample value

         'mov       tmp,dacValue
         'call      #toneOut ' Debugging attach tone detector to output
         
         add       ptr,#1                  ' Bump to next target
         rdlong    tmp,BufferOutEndP1      ' End of buffer
         cmp       ptr,tmp wz              ' Reached end of buffer?
  if_z   rdlong    ptr,BufferOutStart      ' Yes ... reload start
         wrlong    ptr,BufferOutHead       ' New head pointer                     

         ' Check if the head caught the tail
         rdlong    tmp,BufferOutTail       ' Tail value
         cmp       ptr,tmp wz              ' Did the head catch the tail?
  if_nz  jmp       #doD_noError            ' No ... skip error
         rdlong    tmp,BufferOutStatus     ' Increment ...
         add       tmp,#1                  ' ... error ...
         wrlong    tmp,BufferOutStatus     ' ... count            
doD_noError

         ' Start caching next cluster if needed
         sub       outCount,#1 wz          ' Time for a new cluster?
 if_nz   jmp       #doD_noCache            ' No ... skip the cache                                                                                                   
         call      #readToTail             ' Start caching next and advance the tail 2K
         mov       outCount,C_2048         ' 2K bytes till next caching 
doD_noCache

         ' Make a tick and return to top
         movs      doDAC,#doDAC_a          ' Next tick is back at top of play loop
         jmp       #doDAC_DONE             ' Tick the hardware      

      
doADC_IDLE
         rdlong    tmp,BufferInOp          ' Get request
         cmp       tmp,#2 wz               ' Is it a file request?
  if_z   jmp       #doADC_startFile        ' Yes ... go start the file
doA_hlt  mov       tmp,#0                  ' All other ...
         wrlong    tmp,BufferInOp          ' ... requests are cleared
         movs      doADC,#doADC_IDLE       ' Return to IDLE state    
         jmp       #doADC_DONE             ' Move on

doADC_startFile
         rdlong    inMax,BufferInSIZE      ' Remember the maximum size
         mov       tmp,#0                  ' No bytes ...
         wrlong    tmp,BufferInSIZE        ' ... written to file yet
         rdlong    inFirst,BufferInLIST    ' Remember the first cluster
         rdlong    ptr,BufferInStart       ' Start of buffer                     
         wrlong    ptr,BufferInTail        ' Tail at head ... buffer empty 
         mov       tmp,#11                 ' 44 bytes to write in wave header
         mov       tmp2,#C_WAV_HDR         ' Source data
adc_f1   movd      adc_f2,tmp2             ' Set destination
         add       tmp2,#1                 ' Next long in wav header data
adc_f2   wrlong    0,ptr                   ' Write 4 bytes in wav header
         add       ptr,#4                  ' Next 4 bytes
         djnz      tmp,#adc_f1             ' Do all 44                   
         wrlong    ptr,BufferInHead        ' Set head to start of buffer
         mov       inCount,C_2048          ' 2048 - 44 bytes ...
         sub       inCount,#44             ' ... till we write the cluster
         
doADC_a  ' Write loop. Check for end of max size.         
         rdlong    tmp,BufferInSIZE wz     ' Reached max bytes ...
         cmp       tmp,inMax wz            ' ... to write?
  if_z   jmp       #doADC_hlt              ' Yes ... go close the file
         add       tmp,#1                  ' No ... we are about to write one
         wrlong    tmp,BufferInSIZE        ' Bump the write-size

         ' Check for a stop-recording request
         rdlong    tmp,BufferInOP          ' Requested ...
         cmp       tmp,#1 wz               ' ... a stop-recording?
  if_z   jmp       #doADC_hlt              ' Yes ... go close the file
         mov       tmp,#2                  ' Ignore ...
         wrlong    tmp,BufferInOP          ' ... other requests         

         ' Write next sample and bump the head
         rdlong    ptr,BufferInHead        ' Read next ...
         wrbyte    adcValue,ptr            ' ... sample value

         'mov       tmp,adcValue
         'call      #toneOut             ' Debug attach tone detector to input samples
                           
         add       ptr,#1                  ' Bump to next target
         rdlong    tmp,BufferInEndP1       ' End of buffer
         cmp       ptr,tmp wz              ' Reached end of buffer?
  if_z   rdlong    ptr,BufferInStart       ' Yes ... reload start
         wrlong    ptr,BufferInHead        ' New head pointer

         ' Check if the head caught the tail
         rdlong    tmp,BufferInTail        ' Tail value
         cmp       ptr,tmp wz              ' Did the head catch the tail?
  if_nz  jmp       #doA_noError            ' No ... skip error
         rdlong    tmp,BufferInStatus      ' Increment ...
         add       tmp,#1                  ' ... error ...
         wrlong    tmp,BufferInStatus      ' ... count
doA_noError 

         ' Start a write if it is time
         sub       inCount,#1 wz           ' Bump the cache-count
  if_nz  jmp       #adc_1                  ' Still bytes to write ... skip
         call      #writeFromTail          ' Start writing the buffer
         mov       inCount,C_2048          ' 2K more bytes before we write again
adc_1

         ' Make a tick and return to top with next sample
         movs      doADC,#doADC_a          ' Top of loop
         jmp       #doADC_DONE             ' Make the tick and come back

doADC_hlt
          ' Wait for last caching operation to finish
doADCw1  rdlong    tmp,d_command wz        ' ## Keep ticking ...
  if_nz  movs      doADC,#doADCw1          ' ## ... back until ...
  if_nz  jmp       #doADC_DONE             ' ## ... cluster writes

         ' Write any partial cluster in the buffer
         cmp       inCount,#0 wz           ' Any leftover data?
  if_z   jmp       #adc_2                  ' No ... no need to write
         call      #writeFromTail          ' Yes ... write the buffer
doADCw2  rdlong    tmp,d_command wz        ' ## Keep ticking ...
  if_nz  movs      doADC,#doADCw2          ' ## ... back until ...
  if_nz  jmp       #doADC_DONE             ' ## ... cluster writes
adc_2

         ' Load the first cluster of the wav file
         rdlong    ptr,BufferInSTART       ' Start of buffer for scratch        
         wrlong    ptr,d_memoryAddress     ' Address to fill
         wrlong    inFirst,d_sectorAddress ' Cluster number of header
         wrlong    D_RD_COMMAND,d_command  ' Start loading
doADCw3  rdlong    tmp,d_command wz        ' ## Keep ticking ...
  if_nz  movs      doADC,#doADCw3          ' ## ... back until ...
  if_nz  jmp       #doADC_DONE             ' ## ... cluster loads

         ' Add the size to the wav header
         rdlong    ptr,BufferInSTART       ' Where the header is loaded
         add       ptr,#40                 ' Offset to data-size field
         rdlong    tmp,BufferInSIZE        ' Get the size of the data written
         wrlong    tmp,ptr                 ' Fill out data-size field in header
         add       tmp,#36                 ' Add size of header (minus 1st two fields)
         sub       ptr,#36                 ' Back up to chunk-size field
         wrlong    tmp,ptr                 ' Write the chunk-size field

         ' Write the header back to disk
         rdlong    ptr,BufferInSTART       ' Start of buffer for scratch         
         wrlong    ptr,d_memoryAddress     ' Address to fill
         wrlong    inFirst,d_sectorAddress ' Cluster number of header
         wrlong    D_WR_COMMAND,d_command  ' Start the header writing back to disk
doADCw4  rdlong    tmp,d_command wz        ' ## Keep ticking ...
  if_nz  movs      doADC,#doADCw4          ' ## ... back until ...
  if_nz  jmp       #doADC_DONE             ' ## ... cluster writes
         
         ' Finish up the state machine
         jmp       #doA_hlt                ' Clear the command status


' Start disk writing from tail to the cluster number in BufferInLIST.
' BufferInLIST is advanced by 4 (1 cluster).
' The tail is advanced 2048 and wraps if needed.
writeFromTail
         rdlong    ptr,BufferInTail         ' Start of data to write
         wrlong    ptr,d_memoryAddress      ' Tell disk driver about it
         rdlong    tmp,BufferInLIST         ' Cluster to write
         wrlong    tmp,d_sectorAddress      ' Tell disk driver about it
         add       tmp,#1*4                 ' Next time we ...
         wrlong    tmp,BufferInLIST         ' ... write next cluster
         wrlong    D_WR_COMMAND,d_command   ' Tell disk driver to start writing data
         add       ptr,C_2048               ' Advance tail 2K
         rdlong    tmp,BufferInEndP1        ' End of ...
         cmp       ptr,tmp wz               ' ... buffer?
  if_z   rdlong    ptr,BufferInStart        ' Yes ... wrap back around
         wrlong    ptr,BufferInTail         ' New tail pointer
writeFromTail_ret
         ret      

' Start disk reading to tail from cluster number in BufferOutLIST.
' BufferOutLIST is advanced by 4 (1 cluster).
' The tail is advanced 2048 and wraps if needed.
readToTail        
         rdlong    ptr,BufferOutTail       ' Get the current tail value

         wrlong    ptr,d_memoryAddress     ' Set memory address in disk command
         rdlong    tmp,BufferOutLIST       ' Read first sector ...
         wrlong    tmp,d_sectorAddress     ' ... number to command
         wrlong    D_RD_COMMAND,d_command  ' Start the cluster reading         
         add       tmp,#1*4                ' Next cluster ...
         wrlong    tmp,BufferOutList       ' ... to cache              
         
         add       ptr,C_2048              ' Advance tail 2K 
         rdlong    tmp,BufferOutEndP1      ' Have we met the ...        
         cmp       ptr,tmp wz              ' ... end of the buffer?
  if_z   rdlong    ptr,BufferOutStart      ' Yes ... restart at beginning       
         wrlong    ptr,BufferOutTail       ' New tail value         
readToTail_ret
         ret                               ' Done
                
' Wait for ADC to indicate conversion complete.
adc_wait
         mov       tmp,ina
         and       tmp,C_MASK_b wz
  if_nz  jmp       #adc_wait
adc_wait_ret
         ret

' Start the ADC conversion process (takes time).
adc_start
         ' Assert the CONVERTSTART pin
         mov       tmp,C_PIN_MASK
         andn      tmp,C_MASK_v
         mov       outa,tmp 
         ' De-assert the CONVERTSTART pin
         or        tmp,C_MASK_v
         mov       outa,tmp    
         ' Come back later ... conversion might take a few cycles
adc_start_ret
         ret

' Read the ADC converted value to "adcValue".
adc_in
         ' Assert the READ pin
         mov       tmp,C_PIN_MASK
         andn      tmp,C_MASK_r
         mov       outa,tmp         
         or        tmp,C_MASK_r
         ' Read the value         
         mov       adcValue,ina         
         ' De-assert the READ pin
         mov       outa,tmp
         and       adcValue,#$FF 
adc_in_ret
         ret         

' Write the "dacValue" to the DAC hardware.
dac_out
         ' Set data-bus to output
         mov   tmp,#$FF
         or    tmp,C_PIN_MASK
         mov   dira,tmp
         ' Put the data on the bus (all controlls de-asserted)         
         and   dacValue,#$FF
         or    dacValue,C_PIN_MASK
         mov   outa,dacValue
         ' Assert the DAC-start pin
         andn  dacValue,C_MASK_d
         mov   outa,dacValue                                  
         ' De-assert the DAC-start pin
         or    dacValue,C_MASK_d
         mov   outa,dacValue
         and   dacValue,#$FF
         ' Set data-bus to input
         mov   dira,C_PIN_MASK
dac_out_ret
         ret
                              'VBRD_dddddddd
C_PIN_MASK     long   %1011_00000000
C_MASK_d       long   %0001_00000000
C_MASK_r       long   %0010_00000000
C_MASK_b       long   %0100_00000000
C_MASK_v       long   %1000_00000000

C_WAV_HDR      long $46_46_49_52   '52_49_46_46
               long   $00_00_00_00 '24_1F_02_00    Length of data + 36
               long $45_56_41_57   '57_41_56_45
               long $20_74_6D_66   '66_6D_74_20

               long $00_00_00_10   '10_00_00_00
               long $00_01_00_01   '01_00_01_00
               
               'long $00_00_3E_80    ' For ...
               'long $00_00_3E_80    ' ... 16K
               long $00_00_1F_40   '40_1F_00_00   ' For 
               long $00_00_1F_40   '40_1F_00_00   ' ... 8K

               long $00_08_00_01   '01_00_08_00 
               long $61_74_61_64   '64_61_74_61
               long   $00_00_00_00 '00_1F_02_00    Length of data

' 80_000_000 clocks  / second
'      8_000 samples / second
'     -----------------------
'     10_000 clocks / sample
C_RATE_D long      10_000      ' For 8K
C_RATE_C long      8_333       ' For ~9600

C_2048             long 2048

D_WR_COMMAND       long %10_00000100  ' Write 4 sectors (2K)
D_RD_COMMAND       long %01_00000100  ' Read 4 sectors (2K)

timer    long      0
adcValue long      0
dacValue long      0
tmp      long      0
tmp2     long      0
ptr      long      0
outCount long      0
inCount  long      0
inFirst  long      0
inMax    long      0       

' Parameter value pointers

d_command          long 0
d_sectorAddress    long 0
d_memoryAddress    long 0

Mode               long 0
TickCount          long 0
InputValue         long 0
ToneDecodeBuffer   long 0

BufferOutStart     long 0
BufferOutEndP1     long 0                                        
BufferOutHead      long 0
BufferOutTail      long 0
BufferOutStatus    long 0
BufferOutOp        long 0
BufferOutLIST      long 0
BufferOutSIZE      long 0

BufferInStart      long 0
BufferInEndP1      long 0                                        
BufferInHead       long 0
BufferInTail       long 0
BufferInStatus     long 0
BufferInOp         long 0
BufferInLIST       long 0
BufferInSIZE       long 0
