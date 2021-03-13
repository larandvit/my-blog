Title: View Yoosee SD-M5 Doorbell Stream in VLC Media Player
Date: 2021-03-13
Category: Surveillance
Cover: /extra/surveillance-logo.png

[Yoosee SD-M5](https://shop.tuyaoem.com/index.php?_route_=yoosee-1080p-smart-doorbell-camera-poe-wireless) doorbell camera is generic one. It supports Onvif 2.5 Profile S and Real Time Streaming Protocol (RTSP). The doorbell exposes 2 RTSP stream profiles. The first one is high resolution with `rtsp://<IP Address>:554/onvif1` address and the resolution is 1920x1080 5-15 fps. The second one is low resolution with `rtsp://<IP Address>:554/onvif2` address and the resolution is 320x240 5-15 fps.

## Prerequisite

Enable NVR connection and set up the NVR/RTSP password.

1. Open Yoosee application settings. It is tested with version 00.46.00.69 of Yoosee application.

    ![Yoosee application settings]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/yoosee-settings.jpg)</br></br>

2. Go to NVR connection

    * Enable to connect.
    * Change password. Write down the entered password as it will be used in VLC media player.

    ![Yoosee application NVR connection settings]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/yoosee-nvr-connection.jpg)</br></br>

## Windows 10 VLC

The sample has been created in Windows 10 with VLC version 3.0.12.

* Select **Open Network Stream** menu.

    ![Windows 10 VLC Open Network Stream]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/windows-10-vlc-open-network-stream.jpg)</br></br>

* Type in `rtsp://admin:<NVR password>@192.168.0.102/onvif1` replacing **< NVR password >** with the password entered on the step 2 in Prerequisite and your doorbell local IP address.

    ![Windows 10 VLC Open Media]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/windows-10-vlc-open-media.jpg)</br></br>

## Linux VLC

The sample has been created in CentOS Linux release 7.9.2009 (Core) with VLC version 3.0.11.1.

* Select **Open Network Stream** menu.

    ![CentOS 7 VLC Open Network Stream]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/centos-7-vlc-open-network-stream.jpg)</br></br>

* Type in `rtsp://admin:<NVR password>@192.168.0.102/onvif1` replacing **< NVR password >** with the password entered on the step 2 in Prerequisite and your doorbell local IP address.

    ![CentOS 7 VLC Open Media]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/centos-7-vlc-open-media.jpg)</br></br>

## Android VLC

The sample has been created in Android version 10 with VLC version 3.3.4.

* Click **New stream** button

    ![Android 10 VLC Add New stream]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/android-10-vlc-add-new-stream.jpg)</br></br>

* Type in `rtsp://admin:<NVR password>@192.168.0.102/onvif1` replacing **< NVR password >** with the password entered on the step 2 in Prerequisite and your doorbell local IP address.

    ![Android 10 VLC Streams]({static}/images/view-yoosee-sd-m5-doorbell-stream-vlc-media-player/android-10-vlc-streams.jpg)</br></br>

## Access to doorbell outside of your local network

* Open port 554 on your router

* Replace your local IP address with your public IP address.
