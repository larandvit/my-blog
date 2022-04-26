Title: Trigger Events in Aeotec Doorbell 6 in Hubitat
Date: 2022-03-26
Modified: 2022-04-25
Category: Hubitat
Cover: /extra/hubitat-logo.png

[Aeotec Doorbel 6](https://aeotec.com/z-wave-doorbell/) is compatible device with [Hubitat Elevation home automation platform](https://hubitat.com/). Doorbell can work in both modes with the hub or independently, but the device configuration can be done only in Hubitat. However, it adds extra versatility to the device. In case if you need to trigger the button click event, it does not work out of box. To fix this issue, the default device driver should be replaced with new one. A set of steps is pretty straightforward.

##1. Add doorbell to your hub

The setup is completed with the default driver. It might be 1 (siren) or 2 (siren and button) devices shown in **Devices** list.

![Aeotec Doorbell 6 Devices]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-drivers.jpg)</br></br>

##2.Open device details 

**Device information** section reveals a device driver.

![Aeotec Doorbell 6 default driver]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-defailt-driver.jpg)</br></br>

##3. Replace device driver

The new device driver is **Aeotec Siren 6 New**.

![Aeotec Doorbell 6 new driver]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-new-driver.jpg)</br></br>

##4. Save new driver

Click **Save Device** button.

##5. Configure device

Click **Configure** button. It will be added new properties for extra buttons in **Commands** and **Preferences** sections.

![Aeotec Doorbell 6 configure device]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-configure-device.jpg)</br></br>

![Aeotec Doorbell 6 new properties]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-new-properties.jpg)</br></br>

##6. Send notifications for button pushed event

Create a new notification, then select **Button** as a notification device, after select your doorbell, finally key in a button number and type of an event which in our case is `pushed`.

![Aeotec Doorbell 6 send notifications for button pushed event]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-button-pushed-notification.jpg)</br></br>

##7. Send notifications for chime is playing event

Doorbell supports `chime is playing` and `chime is stopped` status events associated with the doorbell button. Those events can be caught in Rule Machine.

Create a new rule machine with Custom Attribute capability, then select `status` attribute, and finally select `playing` or `stopped` value.

![Aeotec Doorbell 6 send notifications for chime is playing event]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-select-trigger-events.jpg)</br></br>

##8. Troubleshooting

If pushed button event is not generated, click **Configure** button in device properties of doorbell.

![Aeotec Doorbell 6 configure device]({static}/images/trigger_events-aeotec-doorbell6-hubitat/aeotec-doorbell6-configure-device.jpg)</br></br>

## Resources
* [Aeotec doorbell 6 Community Forum](https://community.hubitat.com/t/aeotec-doorbell-6/50006)
* [How to Add Devices](https://youtu.be/vzUlxJzeeTc)
* [Hubitat Documentation](https://docs.hubitat.com)

