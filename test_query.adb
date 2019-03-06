-- Remote I/O Device Information Query

WITH Ada.Strings.Fixed;
WITH Ada.Text_IO; USE Ada.Text_IO;

WITH RemoteIO.Client.hidapi;

PROCEDURE test_query IS

  remdev   : RemoteIO.Client.Device;
  channels : RemoteIO.ChannelSets.Set;

BEGIN
  New_Line;
  Put_Line("Remote I/O Device Information Query");
  New_Line;

  -- Create the remote I/O device

  remdev := RemoteIO.Client.hidapi.Create;

  -- Query the firmware version

  Put_Line("Information:        " & remdev.GetVersion);

  -- Query the capability string

  Put_Line("Capabilities:       " & remdev.GetCapability);

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "ADC") /= 0 THEN

    -- Query the available ADC input pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_ADC);

    IF NOT channels.Is_Empty THEN
      Put("Found ADC inputs:  ");

      FOR pin OF channels LOOP
        Put(Integer'Image(pin));
      END LOOP;

      New_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "DAC") /= 0 THEN

    -- Query the available DAC input pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_DAC);

    IF NOT channels.Is_Empty THEN
      Put("Found DAC outputs: ");

      FOR pin OF channels LOOP
        Put(Integer'Image(pin));
      END LOOP;

      New_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "GPIO") /= 0 THEN

    -- Query the available GPIO pins

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_GPIO);

    IF NOT channels.Is_Empty THEN
      Put("Found GPIO pins:   ");

      FOR pin OF channels LOOP
        Put(Integer'Image(pin));
      END LOOP;

      New_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "I2C") /= 0 THEN

    -- Query the available I2C buses

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_I2C);

    IF NOT channels.Is_Empty THEN
      Put("Found I2C buses:   ");

      FOR bus OF channels LOOP
        Put(Integer'Image(bus));
      END LOOP;

      NEW_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "PWM") /= 0 THEN

    -- Query the available PWM outputs

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_PWM);

    IF NOT channels.Is_Empty THEN
      Put("Found PWM outputs: ");

      FOR dev OF channels LOOP
        Put(Integer'Image(dev));
      END LOOP;

      New_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "SPI") /= 0 THEN

    -- Query the available SPI devices

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_SPI);

    IF NOT channels.Is_Empty THEN
      Put("Found SPI devices: ");

      FOR dev OF channels LOOP
        Put(Integer'Image(dev));
      END LOOP;

      New_Line;
    END IF;
  END IF;

  IF Ada.Strings.Fixed.Index(remdev.GetCapability, "DEVICE") /= 0 THEN

    -- Query the available abstract devices

    channels := remdev.GetAvailableChannels(RemoteIO.Channel_Device);

    IF NOT channels.Is_Empty THEN
      Put("Found devices:     ");

      FOR dev OF channels LOOP
        Put(Integer'Image(dev));
      END LOOP;

      New_Line;
    END IF;
  END IF;
END test_query;
