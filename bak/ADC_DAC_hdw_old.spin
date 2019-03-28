CON

' 8_000 samples/sec
' Clock rate is 80_000_000Hz /4
'               20_000_000Hz / 8_000Hz =
'                    2_500 instructions between samples

' 8192 bytes in sample buffer
' 2000 = 0010_0000_0000_0000

' 010_0_0000_0000_0000 ' Read buffer  (1 sec) 40_00_00_00
' 011_0_0000_0000_0000 ' Write buffer (1 sec) 60_00_00_00

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
'                   3..9 internal file mode states                   
'                 10 internal file-mode state (running)
'                 11 means halt at tail 
'                 12 means run free in a loop 
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
         mov       outa,C_PIN_MASK         ' All signals high (active low)
         mov       dira,C_PIN_MASK         ' Control pin directions and bus as input

        ' Copy parameter block
         mov       tmp,par
         mov       value,#d_command
         mov       tmp2,#19   
sam_init movd       smi_ptr,value
         add        value,#1
smi_ptr  mov        0,tmp
         add        tmp,#4
         djnz       tmp2,#sam_init

         ' Init the timer for the first wait 
         mov        timer,cnt
         add        timer,C_RATE 
        
main
         ' Wait for the next 10_000 tick (8K/sec)
         waitcnt   timer,C_RATE                     

         rdlong    opOut,BufferOutOp wz    ' Player halted?
  if_z   jmp       #skipDac                ' Yes ... skip it
         cmp       opOut,#1 wz             ' Halt requested?
  if_z   mov       opOut,#0                ' Yes ... write halt ...
  if_z   wrlong    opOut,BufferOutOp       ' Yes ... value
  if_z   jmp       #skipDac                ' Yes ... skip it

         cmp       opOut,#10 wz, wc
  if_b   jmp       #skipDac                ' Not an active-output state
  
         ' Output next sample
         rdlong    ptr,BufferOutHead
         rdbyte    value,ptr                     
         call      #dac_out         
                   
skipDac

         ' Read next sample
         call      #adc_start
         call      #adc_wait 
         call      #adc_in
                  
         rdlong    opIn,BufferInOp wz      ' Reader halted?
  if_z   jmp       #skipAdc                ' Yes ... skip it
         cmp       opIn,#1 wz              ' Halt requested?
  if_nz  jmp       #skipAdc1               ' No ... skip halt handling
         
         
  if_z   mov       opIn,#0                 ' Yes ... write halt ...  
  if_z   wrlong    opIn,BufferOutOp        ' Yes ... value
  if_z   jmp       #skipAdc                ' Yes ... skip it
  

skipAdc1 cmp       opIn,#10 wz, wc
  if_b   jmp       #skipAdc                ' Not an active-input state
    
         rdlong    ptr,BufferInHead 
         wrbyte    value,ptr 
         
skipAdc

         ' Bump the ring buffer pointers and handle file mode.
         ' The times through these routines change considerably and these are
         ' placed below the ADC/DAC I/O so that the I/O always happens very
         ' consistently right on the tick.
         
         call      #dac_bump
         call      #adc_bump            
         
         jmp       #main
         
adc_wait
         mov  tmp,ina
         and  tmp,C_MASK_b wz
  if_nz  jmp  #adc_wait
adc_wait_ret
         ret

adc_start
         ' Assert the CONVERTSTART pin
         mov   tmp,C_PIN_MASK
         andn  tmp,C_MASK_v
         mov   outa,tmp         

         ' De-assert the CONVERTSTART pin
         or    tmp,C_MASK_v
         mov   outa,tmp

         ' Come back later ... conversion might take a few cycles
adc_start_ret
         ret

adc_in
         ' Assert the READ pin
         mov   tmp,C_PIN_MASK
         andn  tmp,C_MASK_r
         mov   outa,tmp         
         or    tmp,C_MASK_r

         ' Read the value
         mov   value,ina
         
         ' De-assert the READ pin
         mov   outa,tmp
        
adc_in_ret
         ret         

dac_out
         ' Set data-bus to output
         mov   tmp,#$FF
         or    tmp,C_PIN_MASK
         mov   dira,tmp

         ' Put the data on the bus (all controlls de-asserted)
         
         ' TOPHER
         'shl   value,#1
         ' TOPHER
         
         and   value,#$FF
         or    value,C_PIN_MASK
         mov   outa,value         

         ' Assert the DAC-start pin
         andn  value,C_MASK_d
         mov   outa,value
                               
         ' De-assert the DAC-start pin
         or    value,C_MASK_d
         mov   outa,value
         and   value,#$FF

         ' Set data-bus to input
         mov   dira,C_PIN_MASK

dac_out_ret
         ret







' File mode states (reading):
' *** Not started
'     - Set state to initializing
'     - Start the disk loading the first cluster
' *** Initializing
'     - Wait for disk to load first cluster and set state to running
' *** Running
'     - If head is on 2K boundary:
'       - Start disk loading next cluster at tail
'       - Advance tail 2K (remember to wrap)

dac_f_req
         rdlong    tmp,BufferOutStart      ' Start of buffer
         wrlong    tmp,BufferOutHead       ' Set head to start of buffer
         wrlong    tmp,BufferOutTail       ' Tail at head ... buffer empty
         call      #cacheOutTail           ' Start loading the 1st cluster               
         mov       opOut,#3                ' State moves to ...
         wrlong    opOut,BufferOutOp       ' ... INI (waiting on load)         
         jmp       #dac_bump_ret           ' Done (next pass is dac_f_ini)
           
dac_f_ini
         rdlong    tmp,d_command wz        ' Wait for 1st cluster ...
  if_nz  jmp       #dac_bump_ret           ' ... to load

         call      #cacheOutTail           ' Start caching next and advance the tail 2K

         rdlong    ptr,BufferOutHead       ' Read ...
         add       ptr,#40                 ' ... data ...
         rdlong    tmp,ptr                 ' ... length from ...
         add       ptr,#4                  ' ... wav header
         wrlong    ptr,BufferOutHead       ' Head points to data
         wrlong    tmp,BufferOutSIZE       ' Keep up with bytes left to play   

         mov       outCount,C_2048         ' 2K bytes till this cluster is done
         sub       outCount,#44            ' Minus the 44 byte header we skipped                  
                     
         mov       opOut,#4                ' State moves to ...
         wrlong    opOut,BufferOutOp       ' ... RUN (keep buffer filled)
         jmp       #dac_bump_ret           ' And 1st sample plays before next bump
         
dac_f_run  
         rdlong    tmp,BufferOutSIZE       ' Decrement the ...
         sub       tmp,#1 wz               ' ... count (we wrote one before coming here)
         wrlong    tmp,BufferOutSIZE       ' Keep new count
 if_z    jmp       #dec_hlt                ' If we did them all then halt

         sub       outCount,#1 wz          ' Time for a new cluster?
 if_nz   jmp       #dac_b2                 ' No ... advance the head    
                                                                                                   
         call      #cacheOutTail           ' Start caching next and advance the tail 2K
         mov       outCount,C_2048         ' 2K bytes till next caching                  
          
         jmp       #dac_b2                 ' Advance the head normally

dac_bump
         cmp       opOut,#0 wz             ' If we are halted ...
  if_z   jmp       #dac_bump_ret           ' ... then do nothing

         cmp       opOut,#2 wz              ' 2 ...
  if_z   jmp       #dac_f_req               ' ... new file requested
         cmp       opOut,#3 wz              ' 3 ...
  if_z   jmp       #dac_f_ini               ' ... waiting on 1st cluster of file
         cmp       opOut,#4 wz              ' 4 ...
  if_z   jmp       #dac_f_run               ' ... file mode is running

dac_b2   ' Bump head
         rdlong    ptr,BufferOutHead       ' Last sample written here
         add       ptr,#1                  ' Bump to next target
         rdlong    tmp,BufferOutEndP1      ' End of buffer
         cmp       ptr,tmp wz, wc          ' Reached end of buffer?
  if_ae  rdlong    ptr,BufferOutStart      ' Yes ... reload start
         wrlong    ptr,BufferOutHead       ' New head pointer                     

         ' Check if we caught our tail
         rdlong    tmp,BufferOutTail       ' Tail value
         cmp       ptr,tmp wz              ' Did the head catch the tail?
  if_nz  jmp       #dac_bump_ret           ' No ... we are done

         ' Note the catch (probably an error)
         rdlong    tmp,BufferOutStatus     ' Note that ...
         add       tmp,#1                  ' ... the head ...
         wrlong    tmp,BufferOutStatus     ' ... caught the tail

         ' Halt if in "halt at catch"
         cmp       opOut,#5 wz             ' Are we in "halt at tail mode"?
  if_nz  jmp       #dac_bump_ret           ' No ... done
dec_hlt  mov       tmp,#0                  ' Set mode ...
         wrlong    tmp,BufferOutOp         ' ... to halted

dac_bump_ret
         ret     


' Advance output tail by 2K and start loading the next cluster
' at the tail. (No check to see if we passed the head).
cacheOutTail        
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
cacheOutTail_ret
         ret                               ' Done



' File mode states (writing):
' *** Not started
'     - Initialize pointers and set state to initializing
' *** Running
'     - If head is on 2K boundary:
'       - Start disk writing cluster at tail
'       - Advance tail 2K (remember to wrap)         

adc_f_req
         rdlong    inMax,BufferInSIZE       ' Remember the maximum size
         mov       tmp,#0                   ' No bytes ...
         wrlong    tmp,BufferInSIZE         ' ... written to file yet
         rdlong    inFirst,BufferInLIST     ' Remember the first cluster
         rdlong    ptr,BufferInStart        ' Start of buffer                     
         wrlong    ptr,BufferInTail         ' Tail at head ... buffer empty 
         mov       tmp,#11                  ' 44 bytes to write in wave header
         mov       tmp2,#C_WAV_HDR          ' Source data
adc_f1   movd      adc_f2,tmp2              ' Set destination
         add       tmp2,#1                  ' Next long in wav header data
adc_f2   wrlong    0,ptr                    ' Write 4 bytes in wav header
         add       ptr,#4                   ' Next 4 bytes
         djnz      tmp,#adc_f1              ' Do all 44                   
         wrlong    ptr,BufferInHead         ' Set head to start of buffer
         mov       inCount,C_2048           ' 2048 - 44 bytes ...
         sub       inCount,#44              ' ... till we write the cluster
         mov       opIn,#4                  ' State moves to ...
         wrlong    opIn,BufferOutOp         ' ... RUN         
         jmp       #adc_bump_ret            ' Done (next pass is adc_f_run) 

adc_f_run
         rdlong    tmp,BufferInSIZE         ' Bump the ...
         add       tmp,#1                   ' ... input size ...
         wrlong    tmp,BufferInSIZE         ' ... count
         sub       inMax,#1 wz              ' All room taken?
  if_z   mov       opIn,#3                  ' Yes ... next state is finishing up
  if_z   mov       inCount,#1               ' Yes ... force a write now  
         sub       inCount,#1 wz            ' We just read a sample
  if_nz  jmp       #adc_b2                  ' Not time to cache ... keep going
         call      #adc_wr_adv              ' Start the write and advance the tail 
         mov       inCount,C_2048           ' Reset count till next write
         jmp       #adc_b2                  ' Now advance the head


adc_wr_adv
         rdlong    ptr,BufferInTail         ' Start of data to write
         wrlong    ptr,d_memoryAddress      ' Tell disk driver about it
         rdlong    tmp,BufferInLIST         ' Cluster to write
         wrlong    tmp,d_sectorAddress      ' Tell disk driver about it
         add       tmp,#4                   ' Next time we ...
         wrlong    tmp,BufferInLIST         ' ... write next cluster
         wrlong    D_WR_COMMAND,d_command   ' Tell disk driver to start writing data
         add       ptr,C_2048               ' Advance tail 2K
         rdlong    tmp,BufferInEndP1        ' End of ...
         cmp       ptr,tmp wz               ' ... buffer?
  if_z   rdlong    ptr,BufferInStart        ' Yes ... wrap back around
         wrlong    ptr,BufferInTail         ' New tail pointer
adc_wr_adv_ret
         ret     


' TOPHER A LOT OF REWORK HERE
' We need a mechanism to request a stop-recording. The state-machine needs to finish so
' we can't just set it to 1 (halt request). The best way is to hide these internal file
' states from the user. Then the halt-request can be processed by the file-run state.
         
adc_f_fin 
         rdlong    tmp,d_command wz         ' Wait for the last cache to finish
  if_nz  jmp       #adc_bump_ret            ' Come back until it does       
         mov       opIn,#XA                 ' Finished ... move to next state                                                 
         wrlong    inFirst,d_sectorAddress  ' This is where we put the WAV header
         rdlong    ptr,BufferInStart        ' Use the start of ...
         wrlong    ptr,d_memoryAddress      ' ... the buffer as scratch
         wrlong    D_RD_COMMAND,d_command   ' Start the cluster loading
         jmp       #adc_bump_ret            ' Done (come back to adc_f_finXA)

adc_f_finXA
         rdlong    tmp,d_command wz         ' Wait on the cluster to load
  if_nz  jmp       #adc_bump_ret            ' Come back until it does
         mov       opIn,#XB                 ' Finished ... move to next state        
         rdlong    tmp,BufferInSIZE         ' Size of data
         add       ptr,#40                  ' Write ...
         wrlong    tmp,ptr                  ' ... data size
         add       tmp,#36                  ' Chunk size is data size + 36
         sub       ptr,#36                  ' Chunk size pointer
         wrlong    tmp,ptr                  ' Write chunk size               
         rdlong    ptr,BufferInStart        ' Use the start of ...
         wrlong    ptr,d_memoryAddress      ' ... the buffer as scratch
         wrlong    D_WR_COMMAND,d_command   ' Start the cluster writing
         jmp       #adc_bump_ret            ' Done (come back to adc_f_finXC)

adc_f_finXB
         rdlong    tmp,d_command wz         ' Wait on the cluster to write
  if_nz  jmp       #adc_bump_ret            ' Come back until it does
         jmp       #adc_hlt                 ' All done ... halt the channel
       
adc_bump

         cmp       opIn,#0 wz               ' If we are halted ...
  if_z   jmp       #adc_bump_ret            ' ... then do nothing

         cmp       opIn,#2 wz               ' 2 ...
  if_z   jmp       #adc_f_req               ' ... new file requested
         cmp       opIn,#3 wz               ' 3 ...
  if_z   jmp       #adc_f_fin               ' ... waiting on 1st cluster of file
         cmp       opIn,#4 wz               ' 4 ...
  if_z   jmp       #adc_f_run               ' ... file mode is running

adc_b2   ' Bump head
         rdlong    ptr,BufferInHead        ' Last sample written here
         add       ptr,#1                  ' Bump to next target
         rdlong    tmp,BufferInEndP1       ' End of buffer
         cmp       ptr,tmp wz, wc          ' Reached end of buffer?
  if_ae  rdlong    ptr,BufferInStart       ' Yes ... reload start
         wrlong    ptr,BufferInHead        ' New head pointer                     

         ' Check if we caught our tail
         rdlong    tmp,BufferInTail        ' Tail value
         cmp       ptr,tmp wz              ' Did the head catch the tail?
  if_nz  jmp       #adc_bump_ret           ' No ... we are done

         ' Note the catch (probably an error)
         rdlong    tmp,BufferInStatus      ' Note that ...
         add       tmp,#1                  ' ... the head ...
         wrlong    tmp,BufferInStatus      ' ... caught the tail

         ' Halt if in "halt at catch"
         cmp       opIn,#5 wz              ' Are we in "halt at tail mode"?
  if_nz  jmp       #adc_bump_ret           ' No ... done
adc_hlt  mov       tmp,#0                  ' Set mode ...
         wrlong    tmp,BufferInOp          ' ... to halted

adc_bump_ret
         ret     

value   long   0
tmp     long   0
tmp2    long   0
ptr     long   0

opOut   long   0
opIn    long   0
                      'VBRD_dddddddd
C_PIN_MASK     long   %1011_00000000
C_MASK_d       long   %0001_00000000
C_MASK_r       long   %0010_00000000
C_MASK_b       long   %0100_00000000
C_MASK_v       long   %1000_00000000

C_WAV_HDR  long $46_46_49_52 '52_49_46_46
           long   $00_00_00_00 '24_1F_02_00    Length of data + 36
           long $45_56_41_57 '57_41_56_45
           long $20_74_6D_66 '66_6D_74_20

           long $00_00_00_10 '10_00_00_00
           long $00_01_00_01 '01_00_01_00
           long $00_00_1F_40 '40_1F_00_00
           long $00_00_1F_40 '40_1F_00_00

           long $00_08_00_01 '01_00_08_00 
           long $61_74_61_64 '64_61_74_61
           long   $00_00_00_00 '00_1F_02_00    Length of data

' 80_000_000 clocks  / second
'      8_000 samples / second
'     -----------------------
'     10_000 clocks / sample
C_RATE   long 10_000
timer    long 0
outCount long 0

inMax    long 0
inFirst  long 0
inCount  long 0

d_command          long 0
d_sectorAddress    long 0
d_memoryAddress    long 0

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

C_2048             long 2048

D_WR_COMMAND       long %10_00000100  ' Write 4 sectors (2K)
D_RD_COMMAND       long %01_00000100  ' Read 4 sectors (2K)

' For testing
C_ADDR_A       long   $7F_FE
C_ADDR_B       long   $7F_FF