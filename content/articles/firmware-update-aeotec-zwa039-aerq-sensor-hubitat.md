Title: Firmware Update Aeotec ZWA039 aërQ Sensor with Hubitat
Date: 2022-05-22
Category: Hubitat
Cover: /extra/hubitat-logo.png

[Aeotec ZWA039 aërQ Temperature and Humidity Sensor](https://help.aeotec.com/support/solutions/articles/6000227918-a%C3%ABrq-temperature-and-humidity-sensor-user-guide-) is a new device which replaced ZWA009 model. As a new device, it has a defect to report battery level. When the device is added to Habitat, it shows low or zero level of battery. It seems the issue is the firmware installed in the device. Aeotec has released some firmware updates and the latest one should fix the issue with battery.

The sample is based on firmware version 2.02.

##1. Add device with None security

It can be done temporary for duration of a firmware update. Security slows down updates very significantly. I didn't manage to complete my firmware update with S2 security activated.

##2. Disable device

Open device list and click **X** in right top conner. It will add Disable new column.

![Hubitat disable device]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_disable_device.jpg)</br></br>

##3. Install Device Firmware Updater

Go to **Apps** and find **Device Firmware Updater**.

![Hubitat install new built-in app]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_install_new_built_in_app.jpg)</br></br>

##4. Upload Firmware File

The firmware can be downloaded from [Update ZWA039 aerQ Sensor V2.02 ](https://help.aeotec.com/support/solutions/articles/6000256075-update-zwa039-aerq-sensor-v2-02) location.

Make sure to rename the file to remove all spaces and special characters. Original name is ZWA039_AerQ Sensor_US_V2.02.gbl and the new name should be ZWA039_AerQ_Sensor_US_V2.02.gbl.

![Hubitat firmware files]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_firmware_files.jpg)</br></br>

##5. Initiate Update Z-Ware Firmware

After uploading the firmware file, click **Back** button and, then click **Update Z-Ware Firmware** button. You have about 5 seconds to click **Action** button on your device. The button will wake up the device. The spinning circle tells time when **Action** button is supposed to be clicked.

![Hubitat update Z-Ware firmware]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_update_zware_firmware.jpg)</br></br>

##6. Update Firmware Confirmation

The screen reveals read device information. Click **Start Firmware Update**.

![Hubitat update firmware]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_update_firmware.jpg)</br></br>

##7. Firmware Status Updates

A set of stages is followed.

![Hubitat firmware update status starting]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_firmware_update_status_starting.jpg)</br></br>

![Hubitat firmware update status progress]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_firmware_update_status_progress.jpg)</br></br>

![Hubitat firmware update status completed]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_firmware_update_status_completed.jpg)</br></br>

![Hubitat firmware update status last fragement]({static}/images/firmware-update-aeotec-zwa039-aerq-sensor-hubitat/hubitat_firmware_update_status_last_fragement.jpg)</br></br>

##8. Remove and Add Device with Security

## Resources
* [Habitat Device Firmware Updater](https://docs.hubitat.com/index.php?title=Device_Firmware_Updater)
* [Aeotec aerQ ZWave Temperature and Humidity Sensor battery status problems](https://community.hubitat.com/t/aeotec-aerq-zwave-temperature-and-humidity-sensor-battery-status-problems/68047)
* [Disable Device Drivers](https://docs.hubitat.com/index.php?title=Devices#Disable_Device_Drivers)
* [Hubitat Documentation](https://docs.hubitat.com)

