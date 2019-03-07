-- Remote I/O A/D Converter Test

with Ada.Text_IO; use Ada.Text_IO;

with ADC;
with RemoteIO.Client.hidapi;
with RemoteIO.LPC1114;
with Voltage;

procedure test_adc is

   remdev : RemoteIO.Client.Device;
   AD1    : Voltage.Interfaces.Input;

begin
   New_Line;
   Put_Line("Remote I/O A/D Converter Test");
   New_Line;

   -- Open the remote I/O device

   remdev := RemoteIO.Client.hidapi.Create;
   AD1    := ADC.Create(remdev.Create(RemoteIO.LPC1114.AD1), 3.3);

   -- Display analog input voltage

   loop
      Voltage.Volts_IO.Put(AD1.Get);
      New_Line;

      delay 1.0;
   end loop;
end test_adc;
