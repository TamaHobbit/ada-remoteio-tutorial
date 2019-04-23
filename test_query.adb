-- Remote I/O Device Information Query

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with RemoteIO.Client.hidapi;

procedure test_query is

  remdev   : RemoteIO.Client.Device;
  channels : RemoteIO.ChannelSets.Set;

begin
  New_Line;
  Put_Line("Remote I/O Device Information Query");
  New_Line;

  -- Create the remote I/O device

  remdev := RemoteIO.Client.hidapi.Create;

  -- Query the firmware version

  Put_Line("Information:        " & remdev.GetVersion);

  -- Query the capability string

  Put_Line("Capabilities:       " & remdev.GetCapability);

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "ADC") /= 0 then

    -- Query the available ADC input pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_ADC);

    if not channels.Is_Empty then
      Put("Found ADC inputs:  ");

      for pin of channels loop
        Put(Integer'Image(pin));
      end loop;

      New_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "DAC") /= 0 then

    -- Query the available DAC input pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_DAC);

    if not channels.Is_Empty then
      Put("Found DAC outputs: ");

      for pin of channels loop
        Put(Integer'Image(pin));
      end loop;

      New_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "GPIO") /= 0 then

    -- Query the available GPIO pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_GPIO);

    if not channels.Is_Empty then
      Put("Found GPIO pins:   ");

      for pin of channels loop
        Put(Integer'Image(pin));
      end loop;

      New_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "I2C") /= 0 then

    -- Query the available I2C buses

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_I2C);

    if not channels.Is_Empty then
      Put("Found I2C buses:   ");

      for bus of channels loop
        Put(Integer'Image(bus));
      end loop;

      NEW_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "PWM") /= 0 then

    -- Query the available PWM outputs

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_PWM);

    if not channels.Is_Empty then
      Put("Found PWM outputs: ");

      for dev of channels loop
        Put(Integer'Image(dev));
      end loop;

      New_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "SPI") /= 0 then

    -- Query the available SPI devices

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_SPI);

    if not channels.Is_Empty then
      Put("Found SPI devices: ");

      for dev of channels loop
        Put(Integer'Image(dev));
      end loop;

      New_Line;
    end if;
  end if;

  if Ada.Strings.Fixed.Index(remdev.GetCapability, "DEVICE") /= 0 then

    -- Query the available abstract devices

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_Device);

    if not channels.Is_Empty then
      Put("Found devices:     ");

      for dev of channels loop
        Put(Integer'Image(dev));
      end loop;

      New_Line;
    end if;
  end if;
end test_query;
