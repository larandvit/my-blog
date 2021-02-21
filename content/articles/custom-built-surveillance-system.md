Title: Connect Yoosee SD-M5 Doorbell to Synology DSM
Date: 2021-02-21
Category: Surveillance, Synology DSM
Cover: /extra/surveillance-logo.png

[Yoosee SD-M5](https://shop.tuyaoem.com/index.php?_route_=yoosee-1080p-smart-doorbell-camera-poe-wireless) doorbell camera is not included in the list of compatible cameras for Synology DSM. Hopefully, this camera exposes Real Time Streaming Protocol (RTSP) and supports Onvif 2.5 Profile S. It allows connect the doorbell camera to Synology DSM as an Onvif device. A feature of this camera is that it has 5000 fixed port.

The sample has been tested with DSM 6.2.3-25426 Update 3 Synology DSM version and 13.1.1.36 Yoosee camera firmware with Android application.

##1. Open Yoosee application settings

![Yoosee application settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/yoosee-settings.jpg)</br></br>

##2. Go to NVR connection

* Enable to connect.
* Change password. Write down the entered password as it will be used in Synology DSM.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/yoosee-nvr-connection.jpg)</br></br>

##3. Add a new camera 

* Select Complete Setup.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/setup-mode.jpg)</br></br>

##4. Enter connection information

* Camera name.
* Camera IP address.
* Port: 5000.
* Username: admin.
* Password: NVR password changed on step 2.

Click **Test Connection** to validate the information.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/test-connection.jpg)</br></br>

##5. Click Load Capacity

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/load-capability.jpg)</br></br>

##6. Change Stream 1 setting

* Resolution: 1920x1080.
* Frame rate (FPS): 25.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/video-settings.jpg)</br></br>

##7. Leave Recording settings as is

* It can be adjusted later on as per your setup.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/recording-settings.jpg)</br></br>

##8. Leave Schedule setting as is

* It can be adjusted later on as per your setup.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/schedule-settings.jpg)</br></br>

##9. Final screen

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/final-screen.jpg)</br></br>

##10. Activating process

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/activating-camera.jpg)</br></br>

##11. Camera ready

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/connected-camera.jpg)</br></br>

##Troubleshooting

In case of receiving **disconnected** status, try to replace admin password entered on step 4 with NVR one.

![Yoosee application NVR connection settings]({static}/images/connect-yoosee-sd-m5-doorbell-synology-dsm/disconnected-camera.jpg)</br></br>
