-- Minimal binding for the Raspberry Pi LPC1114 I/O Processor Expansion Board
-- Remote I/O Protocol server
--
-- See also: http://git.munts.com/rpi-mcu/expansion/LPC1114/doc/UserGuide.pdf

with Interfaces;

with Message64;
with RemoteIO.Abstract_Device;
with RemoteIO.Client;

package LPC1114 is

   -- Raspberry Pi LPC1114 I/O Processor SPI Agent Firmware command structure

   type Command is record
      command : Interfaces.Unsigned_32;
      pin     : Interfaces.Unsigned_32;
      data    : Interfaces.Unsigned_32;
   end record;

   -- Raspberry Pi LPC1114 I/O Processor SPI Agent Firmware response structure

   type Response is record
      command : Interfaces.Unsigned_32;
      pin     : Interfaces.Unsigned_32;
      data    : Interfaces.Unsigned_32;
      error   : Interfaces.Unsigned_32;
   end record;

   -- Raspberry Pi LPC1114 I/O Processor SPI Agent Firmware commands

   NOP                          : CONSTANT Interfaces.Unsigned_32 := 0;
   LOOPBACK                     : CONSTANT Interfaces.Unsigned_32 := 1;
   CONFIGURE_ANALOG_INPUT       : CONSTANT Interfaces.Unsigned_32 := 2;
   CONFIGURE_GPIO_INPUT         : CONSTANT Interfaces.Unsigned_32 := 3;
   CONFIGURE_GPIO_OUTPUT        : CONSTANT Interfaces.Unsigned_32 := 4;
   CONFIGURE_PWM_OUTPUT         : CONSTANT Interfaces.Unsigned_32 := 5;
   GET_ANALOG                   : CONSTANT Interfaces.Unsigned_32 := 6;
   GET_GPIO                     : CONSTANT Interfaces.Unsigned_32 := 7;
   PUT_GPIO                     : CONSTANT Interfaces.Unsigned_32 := 8;
   PUT_PWM                      : CONSTANT Interfaces.Unsigned_32 := 9;
   CONFIGURE_GPIO_INTERRUPT     : CONSTANT Interfaces.Unsigned_32 := 10;
   CONFIGURE_GPIO               : CONSTANT Interfaces.Unsigned_32 := 11;
   PUT_LEGORC                   : CONSTANT Interfaces.Unsigned_32 := 12;
   GET_SFR                      : CONSTANT Interfaces.Unsigned_32 := 13;
   PUT_SFR                      : CONSTANT Interfaces.Unsigned_32 := 14;
   CONFIGURE_TIMER_MODE         : CONSTANT Interfaces.Unsigned_32 := 15;
   CONFIGURE_TIMER_PRESCALER    : CONSTANT Interfaces.Unsigned_32 := 16;
   CONFIGURE_TIMER_CAPTURE      : CONSTANT Interfaces.Unsigned_32 := 17;
   CONFIGURE_TIMER_MATCH0       : CONSTANT Interfaces.Unsigned_32 := 18;
   CONFIGURE_TIMER_MATCH1       : CONSTANT Interfaces.Unsigned_32 := 19;
   CONFIGURE_TIMER_MATCH2       : CONSTANT Interfaces.Unsigned_32 := 20;
   CONFIGURE_TIMER_MATCH3       : CONSTANT Interfaces.Unsigned_32 := 21;
   CONFIGURE_TIMER_MATCH0_VALUE : CONSTANT Interfaces.Unsigned_32 := 22;
   CONFIGURE_TIMER_MATCH1_VALUE : CONSTANT Interfaces.Unsigned_32 := 23;
   CONFIGURE_TIMER_MATCH2_VALUE : CONSTANT Interfaces.Unsigned_32 := 24;
   CONFIGURE_TIMER_MATCH3_VALUE : CONSTANT Interfaces.Unsigned_32 := 25;
   GET_TIMER_VALUE              : CONSTANT Interfaces.Unsigned_32 := 26;
   GET_TIMER_CAPTURE            : CONSTANT Interfaces.Unsigned_32 := 27;
   GET_TIMER_CAPTURE_DELTA      : CONSTANT Interfaces.Unsigned_32 := 28;
   INIT_TIMER                   : CONSTANT Interfaces.Unsigned_32 := 29;

   -- Raspberry Pi LPC1114 I/O Processor General Purpose Input/Output pins

   LPC1114_GPIO0       : CONSTANT Interfaces.Unsigned_32 := 12; -- aka P1.0
   LPC1114_GPIO1       : CONSTANT Interfaces.Unsigned_32 := 13; -- aka P1.1
   LPC1114_GPIO2       : CONSTANT Interfaces.Unsigned_32 := 14; -- aka P1.2
   LPC1114_GPIO3       : CONSTANT Interfaces.Unsigned_32 := 15; -- aka P1.3
   LPC1114_GPIO4       : CONSTANT Interfaces.Unsigned_32 := 16; -- aka P1.4
   LPC1114_GPIO5       : CONSTANT Interfaces.Unsigned_32 := 17; -- aka P1.5
   LPC1114_GPIO6       : CONSTANT Interfaces.Unsigned_32 := 20; -- aka P1.8
   LPC1114_GPIO7       : CONSTANT Interfaces.Unsigned_32 := 21; -- aka P1.9
   LPC1114_LED         : CONSTANT Interfaces.Unsigned_32 := 7;  -- aka P0.7

   -- Raspberry Pi LPC1114 I/O Processor Analog input pins

   LPC1114_AD1         : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO0;
   LPC1114_AD2         : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO1;
   LPC1114_AD3         : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO2;
   LPC1114_AD4         : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO3;
   LPC1114_AD5         : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO4;

   -- Raspberry Pi LPC1114 I/O Processor PWM output pins

   LPC1114_PWM1        : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO1;
   LPC1114_PWM2        : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO2;
   LPC1114_PWM3        : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO3;
   LPC1114_PWM4        : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO7;

   -- Raspberry Pi LPC1114 I/O Processor Timer pins

   LPC1114_CT32B1_CAP0 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO0;
   LPC1114_CT32B1_MAT0 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO1;
   LPC1114_CT32B1_MAT1 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO2;
   LPC1114_CT32B1_MAT2 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO3;
   LPC1114_CT32B1_MAT3 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO4;
   LPC1114_CT32B0_CAP0 : CONSTANT Interfaces.Unsigned_32 := LPC1114_GPIO5;

   -- LPC1114 special function registers

   DEVICEID            : CONSTANT Interfaces.Unsigned_32 := 16#400483F4#;
   GPIO1DATA           : CONSTANT Interfaces.Unsigned_32 := 16#50010CFC#;
   U0SCR               : CONSTANT Interfaces.Unsigned_32 := 16#4000801C#;

   -- Instantiate RemoteIO.Abstract_Device

   function FromCommand
     (cmd : LPC1114.Command) return Message64.Message;

   function ToResponse
     (msg : Message64.Message) return LPC1114.Response;

   package IOP is new RemoteIO.Abstract_Device
     (LPC1114.Command, LPC1114.Response);

end LPC1114;
