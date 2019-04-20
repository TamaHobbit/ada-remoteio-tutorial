-- Blink an LED on the Remote I/O server(embedded equivalent to "Hello, world")

with Ada.Text_IO; use Ada.Text_IO;

with GPIO.RemoteIO;
with RemoteIO.Client.hidapi;

procedure test_led is

   -- Remote I/O server implementations commonly have GPIO channel 0 dedicated
   -- to an on board LED.  This is not universally implemented, so change the
   -- following constant if necessary.

   Channel_LED : constant := 0;

   remdev : RemoteIO.Client.Device;
   LED    : GPIO.Pin;

begin
   New_Line;
   Put_Line("Remote I/O LED test");
   New_Line;

   remdev := RemoteIO.Client.hidapi.Create;
   LED    := GPIO.RemoteIO.Create(remdev, 0, GPIO.Output);

   loop
      LED.Put(not LED.Get);
      delay 0.5;
   end loop;
end test_led;
