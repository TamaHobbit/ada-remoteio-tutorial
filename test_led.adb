-- Blink an LED on the Remote I/O server (embedded equivalent to "Hello, world")

WITH Ada.Text_IO; USE Ada.Text_IO;

WITH Channels;
WITH GPIO;
WITH HID.hidapi;
WITH RemoteIO.Client;

PROCEDURE test_led IS

  remdev : RemoteIO.Client.Device;
  LED    : GPIO.Pin;

BEGIN
  New_Line;
  Put_Line("Remote I/O LED test");
  New_Line;

  remdev := RemoteIO.Client.Create(HID.hidapi.Create);
  LED    := remdev.Create(Channels.LPC1114_LED, GPIO.Output);

  LOOP
    LED.Put(NOT LED.Get);
    DELAY 0.5;
  END LOOP;
END test_led;
