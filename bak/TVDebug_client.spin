VAR             
  long C_PIN_VID_CLK
  long C_PIN_VID_DATA 
  long C_PIN_KB_CLK  
  long C_PIN_KB_DATA   

PUB c_start(basePin)

  C_PIN_VID_CLK := basePin
  C_PIN_VID_DATA := basePin+1
  C_PIN_KB_CLK := basePin+2
  C_PIN_KB_DATA := basePin+3  

  outa[C_PIN_VID_CLK] :=0
  dira[C_PIN_VID_CLK] := 1
  
  outa[C_PIN_VID_DATA] :=0
  dira[C_PIN_VID_DATA] := 1

  outa[C_PIN_KB_CLK] :=0  
  dira[C_PIN_KB_CLK] := 1
  
  dira[C_PIN_KB_DATA] := 0   

PRI c_read : a | i
  a := 0
  repeat i from 1 to 8
     a := a << 1
     outa[C_PIN_KB_CLK] := 1
     c_pause         
     a := a | ina[C_PIN_KB_DATA]     
     outa[C_PIN_KB_CLK] := 0
     c_pause                          
  
PRI c_send(val) | i, a
  repeat i from 1 to 8
    a := (val >> 7) & 1
    outa[C_PIN_VID_DATA] := a
    outa[C_PIN_VID_CLK] := 1
    c_pause
    outa[C_PIN_VID_CLK] :=0     
    c_pause
    val := val << 1
  c_pause                            

PUB c_pause
     waitcnt(clkfreq / 2000 + cnt)  ' 1/100 of a second

PUB c_reset | i
  repeat i from 1 to 15
    c_send($FF)
  outa[C_PIN_VID_DATA] := 0
  c_pause

PUB c_cls 
  c_send(0)  

PUB c_setcursor(x,y)
  c_send(1)
  c_send(x)
  c_send(y)  
  
PUB c_char(c)
  c_send(2)
  c_send(c) 

PUB c_byte(value) | a
  c_send(3)
  c_send(value)  

PUB c_word(value) | a
  a := value>>8
  c_send(4)
  c_send(a)
  c_send(value)

PUB c_long(value) | a,b,c
  a := value>>24
  b := value>>16
  c := value>>8
  c_send(5)
  c_send(a)
  c_send(b)
  c_send(c)
  c_send(value)

PUB c_string(stringptr) | len
  c_send(6)
  c_send(strsize(stringptr))
  repeat strsize(stringptr)
    c_send(byte[stringptr++]) 

PUB c_getKey : a
  c_send(7) 
  c_pause
  a := c_read
