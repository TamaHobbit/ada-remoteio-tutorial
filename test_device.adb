-- Remote I/O Device Information Query

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with RemoteIO.Client.hidapi;
with RemoteIO_Abstract;

procedure test_device is

   remdev   : RemoteIO.Client.Device;
   absdev   : RemoteIO_Abstract.Device;

begin
   New_Line;
   Put_Line("Remote I/O Abstract Device Information Query");
   New_Line;

   -- Create the remote I/O device

   remdev := RemoteIO.Client.hidapi.Create;
   absdev := RemoteIO_Abstract.Create(remdev, 0);

   Put_Line("Info: " & absdev.GetInfo);
end test_device;
