-- Remote I/O Resources available from a Raspberry Pi LPC1114 I/O Processor
-- Expansion Board (MUNTS-0004)

with RemoteIO;

package Channels is

   -- Analog inputs

   LPC1114_AD1   : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
   LPC1114_AD2   : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
   LPC1114_AD3   : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
   LPC1114_AD4   : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
   LPC1114_AD5   : constant RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4

   -- GPIO pins

   LPC1114_LED   : constant RemoteIO.ChannelNumber := 0;  -- aka LPC1114 P0.7
   LPC1114_GPIO0 : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
   LPC1114_GPIO1 : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
   LPC1114_GPIO2 : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
   LPC1114_GPIO3 : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
   LPC1114_GPIO4 : constant RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4
   LPC1114_GPIO5 : constant RemoteIO.ChannelNumber := 6;  -- aka LPC1114 P1.5
   LPC1114_GPIO6 : constant RemoteIO.ChannelNumber := 7;  -- aka LPC1114 P1.8
   LPC1114_GPIO7 : constant RemoteIO.ChannelNumber := 8;  -- aka LPC1114 P1.9

   -- PWM outputs

   LPC1114_PWM1  : constant RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.1
   LPC1114_PWM2  : constant RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.2
   LPC1114_PWM3  : constant RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.3
   LPC1114_PWM4  : constant RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.9

end Channels;
