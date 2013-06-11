$regfile = "M644def.dat"
$crystal = 16000000
$hwstack = 100
$swstack = 100
$framesize = 100
$baud = 9600

'FOV=180 bedeutet, dass vom startpunkt 90° nach links und 90° nach rechts getrackt werden
Const C_fov = 180
'nullstellung der servos
Dim Servonull As Word
Servonull = 115
'genauigkeit der pan-regelung. hier wird der FOV angegeben. dh bei einem Wert von 20 liegt die Genauigkeit bei ziel+-10
Const Precisionindegrees = 10

'funktionsdeklarationen
Declare Function Isintrackingarea(byval Current_coord As Single , Byval Target_coord As Single , Byval Fov As Single) As Byte
Declare Function Getrotationdirection(byval Current_coord As Single , Byval Target_coord As Single) As Byte
Declare Function Rotate_r(byval Target_coord As Single) As Byte
Declare Function Rotate_l(byval Target_coord As Single) As Byte
Declare Function Getcurrentcoord() As Single
Declare Sub Findnull()

'initialisierung servos
Print "Servo startup..."
Wait 1
Config Servos = 1 , Servo1 = Portd.4 , Reload = 10
Enable Interrupts
Ddrd.4 = 1
Servo(1) = Servonull
Call Findnull()
Print "Servo startup completed..."

'initialisierung i2c
Config Scl = Portc.0
Config Sda = Portc.1
Config I2cdelay = 100
I2cinit

Wait 1
Print "Startup completed..."
Wait 1

'temp kram löschen
Dim Tempgrad As Single
Dim Temp As Byte


'main loop
Do

   Tempgrad = Getcurrentcoord()
   Wait 2

   Temp = Rotate_l(270)

   Wait 2

   Temp = Rotate_r(0)

   Wait 2

   Temp = Rotate_r(90)

   Wait 2

   Temp = Rotate_l(0)

Loop


'stellt fest ob die übergebene koordinate im FOV liegt und somit getrackt werden soll. rückgabe=1 wenn in trackingarea, 0 wenn ausserhalb
Function Isintrackingarea(val Current_coord , Val Target_coord , Val Fov) As Byte
   Local Half_fov As Single
   Local Offset As Single
   Local R_offset As Single

   Half_fov = Fov / 2
   Offset = Current_coord - Half_fov
   Offset = Offset * -1
   R_offset = Target_coord + Offset

   If R_offset < 0 Then
      R_offset = R_offset + 359
   End If

   If R_offset > 359 Then
      R_offset = R_offset - 359
   End If

   If R_offset > 0 And R_offset < Fov Then
      Isintrackingarea = 1
   Else
      Isintrackingarea = 0
   End If

End Function

'gibt die drehrichtung zurück. 0=links, 1=rechts
'TODO: FIXME
Function Getrotationdirection(val Current_coord , Val Target_coord) As Byte
   Local Max_value As Single
   Local Offset As Single

   Max_value = Current_coord + 180
   Offset = 0

   If Max_value > 359 Then
      Max_value = Max_value - 359
   End If

   If Current_coord > Max_value Then
      Offset = 360 - Current_coord
   End If

   Target_coord = Offset + Target_coord

   If Target_coord < Current_coord Then
      Getrotationdirection = 0
   End If

   If Target_coord > Current_coord And Target_coord > Max_value Then
      Getrotationdirection = 0
   End If

   If Target_coord > Current_coord And Target_coord < Max_value Then
      Getrotationdirection = 1
   End If

End Function


'rotiert die pan plattform nach rechts
Function Rotate_r(byval Target_coord As Single)

   Local Current_coord As Single
   Local Position_reached As Byte
   ' default rechtsdrehung
   Local Rotate_dir As Byte
   Rotate_dir = 1

   Position_reached = 0
   Current_coord = Getcurrentcoord()

   Servo(1) = Servonull - 2

   While Position_reached = 0

      Current_coord = Getcurrentcoord()
      Print "calling is intracking| current_coord: " ; Current_coord ; " , target_coord: " ; Target_coord ; " , precision: " ; Precisionindegrees
      Position_reached = Isintrackingarea(current_coord , Target_coord , Precisionindegrees)
      If Position_reached = 1 Then
         Servo(1) = Servonull
         Waitms 200
         Current_coord = Getcurrentcoord()
         Position_reached = Isintrackingarea(current_coord , Target_coord , Precisionindegrees)
         Rotate_dir = Getrotationdirection(current_coord , Target_coord)
         Print "prec check: " ; Position_reached ; " current_coord=" ; Current_coord ; " rotate_dir=" ; Rotate_dir
         Servo(1) = Servonull
      End If
   Wend

   Servo(1) = Servonull

End Function

'rotiert die pan plattform nach links
Function Rotate_l(byval Target_coord As Single)

   Local Current_coord As Single
   Local Position_reached As Byte
   Local Rotate_dir As Byte
   Rotate_dir = 0

   Position_reached = 0
   Current_coord = Getcurrentcoord()
   Servo(1) = Servonull + 3

   While Position_reached = 0

      Current_coord = Getcurrentcoord()
      Print "calling is intracking| current_coord: " ; Current_coord ; " , target_coord: " ; Target_coord ; " , precision: " ; Precisionindegrees
      Position_reached = Isintrackingarea(current_coord , Target_coord , Precisionindegrees)
      If Position_reached = 1 Then
         Servo(1) = Servonull
         Waitms 200
         Current_coord = Getcurrentcoord()
         Position_reached = Isintrackingarea(current_coord , Target_coord , Precisionindegrees)
         Rotate_dir = Getrotationdirection(current_coord , Target_coord)
         Print "prec check: " ; Position_reached ; " current_coord=" ; Current_coord ; " rotate_dir=" ; Rotate_dir
         Servo(1) = Servonull
      End If
   Wend

   Servo(1) = Servonull

End Function


'gibt die aktuelle ausrichtung in grad zurück
Function Getcurrentcoord() As Single
   'Servo(1) = Servonull
   'Waitms 200

   Local Lob As Byte
   Local Hib As Byte
   Local Cmps_slaveid As Byte
   Local Cmps_slaveid_read As Byte
   Local R_value As Integer

   Cmps_slaveid = &HC0
   Cmps_slaveid_read = Cmps_slaveid + 1

   'Register auswählen
   I2cstart
   I2cwbyte Cmps_slaveid
   I2cwbyte 2
   I2cstop

   I2cstart
   I2cwbyte Cmps_slaveid_read
   I2crbyte Hib , Ack
   I2crbyte Lob , Nack
   I2cstop

   R_value = Makeint(lob , Hib)
   Getcurrentcoord = R_value / 10
End Function

' nullstellung für panservo finden
Sub Findnull()
   Local _startpan As Single
   Local _endpan As Single
   Local _tmp As Word

   Local Offset As Word


   Servo(1) = Servonull
   _startpan = Getcurrentcoord()

   ' dieser block detektiert einen drift nach LINKS.
   If _startpan > 0 And _startpan < 180 Then
      Offset = _startpan * -1
   End If

   Waitms 500

   _endpan = Getcurrentcoord()
   _endpan = _endpan + Offset
   If _endpan < 0 Then
      _endpan = 359 + _endpan
   End If

   ' hier drift nach links erkannt
   If _endpan > 0 Then
      Servonull = Servonull - 1
      Print "drift links ausgegliechen"
   End If



End Sub


End
