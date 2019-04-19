-- Remote I/O Device Information Query

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

with Message64;
with RemoteIO.Abstract_Device;
with RemoteIO.Client.hidapi;

with SPI_Agent.Commands;
with SPI_Agent.Messages;

use type Interfaces.Unsigned_32;
use type Message64.Byte;

procedure test_device is

   -- Split 32-bit word into individual bytes

   function ToByte(u : Interfaces.Unsigned_32; b : Natural) return Message64.Byte is

   begin
     return Message64.Byte(Interfaces.Shift_Right(u, b*8) and 16#FF#);
   end ToByte;

   -- Merge four bytes into a 32-bit word

   function ToUnsigned
    (byte0 : Message64.Byte;
     byte1 : Message64.Byte;
     byte2 : Message64.Byte;
     byte3 : Message64.Byte) return Interfaces.Unsigned_32 is

   begin
     return
       Interfaces.Shift_Left(Interfaces.Unsigned_32(byte3), 24) or
       Interfaces.Shift_Left(Interfaces.Unsigned_32(byte2), 16) or
       Interfaces.Shift_Left(Interfaces.Unsigned_32(byte1), 8)  or
       Interfaces.Unsigned_32(byte0);
   end ToUnsigned;

   function FromCommand
    (cmd : SPI_Agent.Messages.SPIAGENT_COMMAND_MSG_t) return Message64.Message is

   begin
     return
      (3  => ToByte(cmd.Command, 3),
       4  => ToByte(cmd.Command, 2),
       5  => ToByte(cmd.Command, 1),
       6  => ToByte(cmd.Command, 0),

       7  => ToByte(cmd.Pin, 3),
       8  => ToByte(cmd.Pin, 2),
       9  => ToByte(cmd.Pin, 1),
       10 => ToByte(cmd.Pin, 0),

       11 => ToByte(cmd.Data, 3),
       12 => ToByte(cmd.Data, 2),
       13 => ToByte(cmd.Data, 1),
       14 => ToByte(cmd.Data, 0),

       others => 0);
   end FromCommand;

   function ToResponse(msg : Message64.Message) return SPI_Agent.Messages.SPIAGENT_RESPONSE_MSG_t is

   begin
     return
      (ToUnsigned(msg(6),  msg(5),  msg(4),  msg(3)),
       ToUnsigned(msg(10), msg(9),  msg(8),  msg(7)),
       ToUnsigned(msg(14), msg(13), msg(12), msg(11)),
       ToUnsigned(msg(18), msg(17), msg(16), msg(15)));
   end ToResponse;

   package IO_Processor is new RemoteIO.Abstract_Device
    (SPI_Agent.Messages.SPIAGENT_COMMAND_MSG_t,
     SPI_Agent.Messages.SPIAGENT_RESPONSE_MSG_t);

   package UnsignedIO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);
   use UnsignedIO;

   remdev : RemoteIO.Client.Device;
   iopdev : IO_Processor.Device;
   cmd    : SPI_Agent.Messages.SPIAGENT_COMMAND_MSG_t;
   resp   : SPI_Agent.Messages.SPIAGENT_RESPONSE_MSG_t;

begin
   New_Line;
   Put_Line("Remote I/O Abstract Device Information Query");
   New_Line;

   -- Create the remote I/O device

   remdev := RemoteIO.Client.hidapi.Create;
   iopdev := IO_Processor.Create(remdev, 0);

   -- Display the abstract device information string

   Put_Line("Abstract Device Info:     " & iopdev.GetInfo);

   -- Display the LPC1114 I/O Processor firmware version

   cmd.Command := SPI_Agent.Commands.SPIAGENT_COMMAND_t'POS(SPI_Agent.Commands.SPIAGENT_CMD_NOP);
   cmd.Pin     := 0;
   cmd.Data    := 0;

   iopdev.Operation(cmd, resp);

   Put_Line("LPC1114 Firmware Version:" & resp.Data'Image);

   -- Display the LPC1114 chip ID

   Put("LPC1114 Device ID:        ");

   cmd.Command := SPI_Agent.Commands.SPIAGENT_COMMAND_t'POS(SPI_Agent.Commands.SPIAGENT_CMD_GET_SFR);
   cmd.Pin     := 16#400483F4#;
   cmd.Data    := 0;

   iopdev.Operation(cmd, resp);

   Put(resp.Data, 12, 16);
   New_Line;
end test_device;
