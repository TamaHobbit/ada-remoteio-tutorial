-- USB HID Remote I/O A/D Converter Test

WITH Ada.Text_IO; USE Ada.Text_IO;

WITH ADC;
WITH RemoteIO.Client.hidapi;
WITH Voltage;

PROCEDURE test_adc IS

  remdev : RemoteIO.Client.Device;
  AIN0   : Voltage.Interfaces.Input;

BEGIN
  New_Line;
  Put_Line("USB HID Remote I/O A/D Converter Test");
  New_Line;

  -- Open the remote I/O device

  remdev := RemoteIO.Client.hidapi.Create;
  AIN0   := ADC.Create(remdev.Create(1), 3.3);

  -- Display analog input voltage

  LOOP
    Voltage.Volts_IO.Put(AIN0.Get);
    New_Line;

    DELAY 1.0;
  END LOOP;
END test_adc;
