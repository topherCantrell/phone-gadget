CON

' 0 CLS
' 1 x y
' 2 char
' 3 a
' 4 a_b
' 5 a_b_c_d
' 6 len ss...
' 7 getKey

  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000

  S_PIN_VID_CLK  = 0
  S_PIN_VID_DATA = 1
  S_PIN_KB_CLK   = 2
  S_PIN_KB_DATA  = 3    

OBJ     
  text : "tv_text"
  kb   : "keyboard"

VAR             
  long C_PIN_VID_CLK
  long C_PIN_VID_DATA 
  long C_PIN_KB_CLK  
  long C_PIN_KB_DATA

  long FFCount
  
PUB s_start | i, a

  FFCount := 0
  
  dira[S_PIN_VID_CLK] := 0
  
  dira[S_PIN_VID_DATA] := 0
  
  dira[S_PIN_KB_CLK] := 0
  
  outa[S_PIN_KB_DATA] := 0
  dira[S_PIN_KB_DATA] := 1

  ' start video text
  text.start(12)

  'start the keyboard
  kb.start(26, 27)
      
  repeat
    a := s_receive  
    case a
      $00: s_0_cls
      $01: s_1_setcursor
      $02: s_2_char
      $03: s_3_byte
      $04: s_4_word
      $05: s_5_long
      $06: s_6_string
      $07: s_7_getKey
      other:
          text.str(string("<??"))
          text.hex(a,2)
          text.str(string("??>"))      

PUB s_send(value) | a , i
     repeat i from 1 to 8
      repeat while ina[S_PIN_KB_CLK]==0
      a := (value >> 7)&1
      outa[S_PIN_KB_DATA] := a
      value := value<<1
      repeat while ina[S_PIN_KB_CLK] <> 0
         
PUB s_receive : a | i
    a := 0
    repeat i from 1 to 8
      repeat while ina[S_PIN_VID_CLK]==0
      a := a << 1
      a := a + ina[S_PIN_VID_DATA]     
      repeat while ina[S_PIN_VID_CLK] <> 0

    if (a==$FF)
      ++FFCount
    else
      FFCount := 0
      
    if (a==$FF) AND (FFCount>10)
      repeat while ina[S_PIN_VID_DATA] == 1
      FFCount := 0
      a := s_receive
      
PUB s_0_cls
  text.out(0)

PUB s_1_setcursor | x, y
  x := s_receive
  y := s_receive
  text.out($A)
  text.out(x)
  text.out($B)
  text.out(y)
  
PUB s_2_char | c
  c := s_receive
  text.out(c)

PUB s_3_byte | a
  a := s_receive
  text.hex(a,2)

PUB s_4_word | a,b
  a := s_receive
  b := s_receive
  text.hex(a,2)
  text.hex(b,2)

PUB s_5_long | a,b,c,d
  a := s_receive
  b := s_receive
  c := s_receive
  d := s_receive
  text.hex(a,2)
  text.hex(b,2)
  text.hex(c,2)
  text.hex(d,2)

PUB s_6_string | a, i, len
  len := s_receive
  repeat i from 1 to len
    a := s_receive
    text.out(a)

PUB s_7_getKey | a
  a := kb.key
  s_send(a)
  
 