-- I/O Resources available from a Raspberry Pi LPC1114 I/O Processor
-- Expansion Board (MUNTS-0004)

-- Copyright (C)2019, Philip Munts, President, Munts AM Corp.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

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

  LPC1114_PWM1  : CONSTANT RemoteIO.ChannelNumber := 1;	-- aka LPC1114 P1.1
  LPC1114_PWM2  : CONSTANT RemoteIO.ChannelNumber := 2;	-- aka LPC1114 P1.2
  LPC1114_PWM3  : CONSTANT RemoteIO.ChannelNumber := 3;	-- aka LPC1114 P1.3
  LPC1114_PWM4  : CONSTANT RemoteIO.ChannelNumber := 4;	-- aka LPC1114 P1.9

END Channels;
