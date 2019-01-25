-- Remote I/O Resources available from a Raspberry Pi LPC1114 I/O Processor
-- Expansion Board (MUNTS-0004)

WITH RemoteIO;

PACKAGE Channels IS

  -- Analog inputs

  LPC1114_AD1   : CONSTANT RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
  LPC1114_AD2   : CONSTANT RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
  LPC1114_AD3   : CONSTANT RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
  LPC1114_AD4   : CONSTANT RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
  LPC1114_AD5   : CONSTANT RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4

  -- GPIO pins

  LPC1114_LED   : CONSTANT RemoteIO.ChannelNumber := 0;  -- aka LPC1114 P0.7
  LPC1114_GPIO0 : CONSTANT RemoteIO.ChannelNumber := 1;  -- aka LPC1114 P1.0
  LPC1114_GPIO1 : CONSTANT RemoteIO.ChannelNumber := 2;  -- aka LPC1114 P1.1
  LPC1114_GPIO2 : CONSTANT RemoteIO.ChannelNumber := 3;  -- aka LPC1114 P1.2
  LPC1114_GPIO3 : CONSTANT RemoteIO.ChannelNumber := 4;  -- aka LPC1114 P1.3
  LPC1114_GPIO4 : CONSTANT RemoteIO.ChannelNumber := 5;  -- aka LPC1114 P1.4
  LPC1114_GPIO5 : CONSTANT RemoteIO.ChannelNumber := 6;  -- aka LPC1114 P1.5
  LPC1114_GPIO6 : CONSTANT RemoteIO.ChannelNumber := 7;  -- aka LPC1114 P1.8
  LPC1114_GPIO7 : CONSTANT RemoteIO.ChannelNumber := 8;  -- aka LPC1114 P1.9

  -- PWM outputs

  LPC1114_PWM1  : CONSTANT RemoteIO.ChannelNumber := 1;	 -- aka LPC1114 P1.1
  LPC1114_PWM2  : CONSTANT RemoteIO.ChannelNumber := 2;	 -- aka LPC1114 P1.2
  LPC1114_PWM3  : CONSTANT RemoteIO.ChannelNumber := 3;	 -- aka LPC1114 P1.3
  LPC1114_PWM4  : CONSTANT RemoteIO.ChannelNumber := 4;	 -- aka LPC1114 P1.9

END Channels;
