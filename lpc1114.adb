-- Minimal binding for the Raspberry Pi LPC1114 I/O Processor Expansion Board
-- Remote I/O Protocol server
--
-- See also: http://git.munts.com/rpi-mcu/expansion/LPC1114/doc/UserGuide.pdf

with Interfaces;

use type Interfaces.Unsigned_32;

package body LPC1114 is

   -- Split 32-bit word into individual bytes

   function ToByte
     (u : Interfaces.Unsigned_32; b : Natural) return Message64.Byte is

   begin
      return Message64.Byte(Interfaces.Shift_Right(u, b*8) and 16#FF#);
   end ToByte;

   -- Merge four bytes into one 32-bit word

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

   -- Convert command message

   function FromCommand
     (cmd : LPC1114.Command) return Message64.Message is

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

         others     => 0);
   end FromCommand;

   -- Convert response message

   function ToResponse
     (msg : Message64.Message) return LPC1114.Response is

   begin
      return
        (ToUnsigned(msg(6),  msg(5),  msg(4),  msg(3)),
         ToUnsigned(msg(10), msg(9),  msg(8),  msg(7)),
         ToUnsigned(msg(14), msg(13), msg(12), msg(11)),
         ToUnsigned(msg(18), msg(17), msg(16), msg(15)));
   end ToResponse;

end LPC1114;
