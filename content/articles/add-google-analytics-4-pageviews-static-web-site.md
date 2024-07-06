Title: Add Google Universal Analytics 4 Pageviews in Static Web Site
Date: 2024-07-05
Category: Google Analytics
Cover: /extra/google-analytics-logo.png

Google Analytics 4 exposes data/reports through Google Analytics APIs. It can be used a wide variety of languages to access Google Analytics. Implementation of Google Analytics can be done only on server side which does not allow adding it to a static Web site. However, it is still possible to bypass this limitation. An idea is to generate page views data by means of a service application on schedule and store it in a JavaScript file as a variable. After that, a static Web site can easily show it on each page.

A sample of a solution is located in [github](https://github.com/larandvit/google-analytics4-pageview-counter) repository. Static web site implementation is [Pelican blog](https://techjogging.com/create-keytab-file-for-kerberos-authentication-in-windows.html).

Assuming that Google Analytics 4 has been set up already.

## 1. Complete prerequisites in Google Analytics

Follow steps ##1-2 from [API Quickstart](https://developers.google.com/analytics/devguides/reporting/data/v1/quickstart-client-libraries) guide.

The objectives are

   * Create a new project.
   * Enable APIs.
   * Create a service account.
   * Download credentials.json file.
   * Add service account to the Google Analytics 4 property.

## 2. Develop Generate Google Analytics data tool

As a language is chosen Python. 

Install Google Analytics library.

    :::python
    pip install google-analytics-data

The application code can be found in [file](https://github.com/larandvit/google-analytics4-pageview-counter/blob/main/generate_page_views_analytics4.py).

## 3. Set up the tool

Google Analytics APIs need to know the location of credentials file. Replace the path with a location of your file.

    :::bash
    export GOOGLE_APPLICATION_CREDENTIALS="/opt/generate_page_views_analytics4/credentials.json"

##4. Run the tool

    :::bash
    python3 /opt/generate_page_views_analytics4/generate_page_views_analytics4.py -p 123456789 -o /var/www/sample.com/blog_metrics.js

Parameters

   * 123456789 - your Google Analytics PROPERTY_ID.
   * /var/www/sample.com/blog_metrics.js - location where you store your generated Google Analytics data.

Generated file sample represents JavaScript Map constant with page names and number of views. The name is blog_metrics.js.

    :::json
    const blog_views = new Map([
       ["/show_pageviews1.html", 5215]
      ,["/show_pageviews2.html", 3212]
      ,["/show_pageviews.html", 2479]
    ]);

## 4. Develop JavaScript code to extract pageviews

The file name might be pageviews.js. It will be embedded in each of your Web pages.

    :::javascript
    window.addEventListener('DOMContentLoaded', function () {
	pagePath = window.location.pathname;
	pageViewsAnalytics4 = 0;
	
	if (blog_views.get(pagePath)) {
		pageViewsAnalytics4 = blog_views.get(pagePath); 
	}
	
	document.getElementById('query-output').value = pageViewsAnalytics4;
	
    });

## 5. Create a HTML file to show pageviews

    :::html

    <!DOCTYPE html>
    <html>
    <head>
      <meta charset="utf-8">
      <title>Google Analytics Pageviews Sample</title>
  
    </head>
    <body>

    <h1>Google Analytics 4</h1>

    <h3>Page Views Counter</h3>
    <textarea cols="10" rows="1" id="query-output"></textarea>

    <script src='blog_metrics.js'></script>

    <script src='pageviews.js'></script>

    </body>
    </html>

## 6. Schedule Generate Google Analytics Data tool to be run every hour

It can be done in many different ways, for example, Linux - cron, Windows - Task Scheduler, Synology DSM - Task Scheduler.

## Resources

* [API Quickstart](https://developers.google.com/analytics/devguides/reporting/data/v1/quickstart-client-libraries)
