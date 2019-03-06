-- Remote I/O Resources available from a Raspberry Pi LPC1114 I/O Processor
-- Expansion Board (MUNTS-0004)

with RemoteIO;

package LPC1114 is

   -- Analog inputs

   AD1   : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
   AD2   : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
   AD3   : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
   AD4   : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
   AD5   : constant RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4

   -- GPIO pins

   LED   : constant RemoteIO.ChannelNumber := 0;  -- aka LPC1114 P0.7
   GPIO0 : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
   GPIO1 : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
   GPIO2 : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
   GPIO3 : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
   GPIO4 : constant RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4
   GPIO5 : constant RemoteIO.ChannelNumber := 6;  -- aka LPC1114 P1.5
   GPIO6 : constant RemoteIO.ChannelNumber := 7;  -- aka LPC1114 P1.8
   GPIO7 : constant RemoteIO.ChannelNumber := 8;  -- aka LPC1114 P1.9

   -- PWM outputs

   PWM1  : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.1
   PWM2  : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.2
   PWM3  : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.3
   PWM4  : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.9

end LPC1114;
