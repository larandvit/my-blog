Title: Redirect 404 Error to Specified URL in Synology DSM
Date: 2020-04-03
Category: Synology DSM
Cover: /extra/synology-logo.png

Synology DiskStation Manager (DSM) restricts flexibility of Web Station customization. Access to many settings in Apache or Nginx Web servers are hidden or not available. There are some open doors to accomplish your customization without breaking Synology DSM. If you want to address 404 error with your way, it's still possible. The sample below is based on Synology DSM 6.2.2. and it shows 404 error page, and then load a Web page of your choice. You can customize the logic eliminating showing of 404 error page at all and go straight to your page.

## 1. Create a file with **missing** name.

Don't use any extensions in file name. It has to be the bare name. The file has to be place in root of your Web site.

## 2. Add HTML content in the **missing** file.

The code is extracted from Synology DSM 404 error page.

    :::html
    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8">
        <meta http-equiv = "refresh" content = "2; url = https://techjogging.com" />
        <style>html{height:100%}body{margin:0 auto;min-height:600px;min-width:800px;height:100%}.top{height:100px;height:calc(40% - 140px)}.bottom{height:150px;height:calc(60% - 210px)}.center{height:350px;text-align:center;vertical-align:middle;font- family:Verdana}.circle{margin:auto;width:260px;height:260px;border-radius:50%;background:#c0c6cc}.circle_text{line-height:260px;font-size:100px;color:#ffffff;font-weight:bold}.text{line-height:40px;font-size:26px;color:#505a64}
        </style>
      </head>
      <body>
        <div class="top"></div>
        <div class="center">
        <div class="circle">
        <div class="circle_text">404</div>
        </div>
        <div>
        <p class="text" id="a">The page you are looking for cannot be found</p>
        </div>
        <div class="bottom"></div>
      </body>
    </html>

## 3. Replace parameters.

    :::html
    <meta http-equiv = "refresh" content = "2; url = https://techjogging.com" />

* Delay before redirecting to your URL.

* Your URL.
