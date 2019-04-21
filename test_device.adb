-- Remote I/O Abstract Device Information Query Test

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

with RemoteIO.Client.hidapi;
with RemoteIO.LPC1114;

procedure test_device is

   package Unsigned_32_IO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);

   remdev : RemoteIO.Client.Device;
   absdev : RemoteIO.LPC1114.Abstract_Device.Device;
   cmd    : RemoteIO.LPC1114.SPIAGENT_COMMAND_MSG_t;
   resp   : RemoteIO.LPC1114.SPIAGENT_RESPONSE_MSG_t;

begin
   New_Line;
   Put_Line("Remote I/O Abstract Device Information Query");
   New_Line;

   -- Create the remote I/O device

   remdev := RemoteIO.Client.hidapi.Create;
   absdev := RemoteIO.LPC1114.Abstract_Device.Create(remdev, 0);

   -- Display the abstract device information string

   Put_Line("Abstract Device Info:     " & absdev.GetInfo);

   -- Display the LPC1114 I/O Processor firmware version

   cmd.Command := RemoteIO.LPC1114.SPIAGENT_CMD_NOP;
   cmd.Pin     := 0;
   cmd.Data    := 0;

   absdev.Operation(cmd, resp);

   Put_Line("LPC1114 Firmware Version:" &
     Interfaces.Unsigned_32'Image(resp.Data));

   -- Display the LPC1114 chip ID

   Put("LPC1114 Device ID:        ");

   cmd.Command := RemoteIO.LPC1114.SPIAGENT_CMD_GET_SFR;
   cmd.Pin     := RemoteIO.LPC1114.LPC1114_DEVICEID;
   cmd.Data    := 0;

   absdev.Operation(cmd, resp);

   Unsigned_32_IO.Put(resp.Data, 12, 16);
   New_Line;
   New_Line;
end test_device;
