-- Blink an LED on the Remote I/O server(embedded equivalent to "Hello, world")

with Ada.Text_IO; use Ada.Text_IO;

with GPIO;
with RemoteIO.Client.hidapi;

procedure test_led is

   remdev : RemoteIO.Client.Device;
   LED    : GPIO.Pin;

begin
   New_Line;
   Put_Line("Remote I/O LED test");
   New_Line;

   remdev := RemoteIO.Client.hidapi.Create;
   LED    := remdev.Create(0, GPIO.Output);

   loop
      LED.Put(not LED.Get);
      delay 0.5;
   end loop;
end test_led;
