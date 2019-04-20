-- Remote I/O Abstract Device Information Query Test

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

with LPC1114;
with RemoteIO.Client.hidapi;

procedure test_device is

   package Unsigned_32_IO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);

   remdev : RemoteIO.Client.Device;
   iopdev : LPC1114.IOP.Device;
   cmd    : LPC1114.Command;
   resp   : LPC1114.Response;

begin
   New_Line;
   Put_Line("Remote I/O Abstract Device Information Query");
   New_Line;

   -- Create the remote I/O device

   remdev := RemoteIO.Client.hidapi.Create;
   iopdev := LPC1114.IOP.Create(remdev, 0);

   -- Display the abstract device information string

   Put_Line("Abstract Device Info:     " & iopdev.GetInfo);

   -- Display the LPC1114 I/O Processor firmware version

   cmd.Command := LPC1114.NOP;
   cmd.Pin     := 0;
   cmd.Data    := 0;

   iopdev.Operation(cmd, resp);

   Put_Line("LPC1114 Firmware Version:" &
     Interfaces.Unsigned_32'Image(resp.Data));

   -- Display the LPC1114 chip ID

   Put("LPC1114 Device ID:        ");

   cmd.Command := LPC1114.GET_SFR;
   cmd.Pin     := LPC1114.DEVICEID;
   cmd.Data    := 0;

   iopdev.Operation(cmd, resp);

   Unsigned_32_IO.Put(resp.Data, 12, 16);
   New_Line;
   New_Line;
end test_device;
