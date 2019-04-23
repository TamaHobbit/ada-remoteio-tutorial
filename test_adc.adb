-- Remote I/O Analog Input Test

with Ada.Text_IO; use Ada.Text_IO;

with ADC.RemoteIO;
with RemoteIO.Client.hidapi;
with RemoteIO.LPC1114;
with Voltage;

procedure test_adc is

  remdev : RemoteIO.Client.Device;
  Vin    : Voltage.Interfaces.Input;

begin
  New_Line;
  Put_Line("Remote I/O Analog Input Test");
  New_Line;

  -- Open the remote I/O device

  remdev := RemoteIO.Client.hidapi.Create;
  Vin    := ADC.Create(ADC.RemoteIO.Create(remdev, RemoteIO.LPC1114.AIN1), 3.3);

  -- Display analog Vinut voltage

  loop
    Voltage.Volts_IO.Put(Vin.Get);
    New_Line;

    delay 1.0;
  end loop;
end test_adc;
