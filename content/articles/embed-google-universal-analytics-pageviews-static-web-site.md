Title: Embed Google Universal Analytics Pageviews in Static Web Site
Date: 2024-07-07
Category: Google Analytics
Cover: /extra/google-analytics-logo.png

Google Universal Analytics was completely retired on Jul 1, 2024. Google suggested to export your reports to files. It helps store your Google Analytics data for future usage. If you follow the recommendation, you can embed pageviews in your static Web site. Your report should include two fields: Page and PageViews.

A sample of a solution is located in [github](https://github.com/larandvit/google-analytics4-pageview-counter) repository. Static web site implementation is [Pelican blog](https://techjogging.com/create-keytab-file-for-kerberos-authentication-in-windows.html).

## 1. Refine your export data.

Exported data might need to be cleaned up as Page field can contain values with extra parameters. For this purpose, file can be imported into SQL Server table. It will allow to manipulate with your data.

Some of anomalies for Page field are.

    :::text
    /access-windows-shared-folder-from-centos.html?_x_tr_sl=en&_x_tr_tl=id&_x_tr_hl=id&_x_tr_pto=tc,1,1,1,1,100.00%,1,1,1
    /apis/site/proxy?url=https://techjogging.com/redirect-404-error-to-specified-url-in-synology-dsm.html,1,1,1,2,0.00%,1,1,0
    /cron-scheduler-with-docker-container-in-centosrhel-7.html?_sc_token=v2:66hsWKis0p5eP_T8n6WHe2aXSZRrMoOpx528AIG-QBiySgJ0dZoui3yghQ5vqHYAf8z242BIQ2UaCTzWLxVLV9LVSyAR1YGYgjpyDSRhrW09NHIFjMdGpvy_8wIxigRm1Xe4xIszbp18x3AvsjUwciUKIFMEi65FHsgtoKTjmU3Lf0SYq_e3o8bFEHe-JV3G,1,1,1,1,100.00%,1,1,0

The expectation for Page field is.

    :::text
    /create-keytab-file-for-kerberos-authentication-in-windows.html
    /access-windows-shared-folder-from-centos.html
    /create-keytab-file-kerberos-authentication-linux.html

Also, PageView field is exported with thousands separator. The separator must be removed to covert the field in numeric data type because data for the same URL should be summed.

Final step is to make Map constant for JavaScript code.

To refine data, a query can be used. Replace Web address with yours and filter pages which you want to exclude. Make your data as less as possible to speed up your Web site.

    :::sql
    WITH extract_page_url AS (
    SELECT 
        Page, 
        CASE
            WHEN CHARINDEX('.html', Page)=0 THEN NULL
            WHEN CHARINDEX('techjogging.com', Page)=0 THEN SUBSTRING(Page,1,CHARINDEX('.html', Page)+4)
            ELSE SUBSTRING(Page,(CHARINDEX('techjogging.com', Page)+15),CHARINDEX('.html', Page)+4-(CHARINDEX('techjogging.com', Page)+15)+1)
        END AS PageUrl,
        CAST(replace(Pageviews,',','') AS int) AS Pageviews
    FROM project.dbo.pageviews_raw)
    , refined_summary AS (
    SELECT
        PageUrl,
        SUM(Pageviews) AS Pageviews
    FROM 
        extract_page_url
    WHERE
        PageUrl IS NOT NULL
        AND PageUrl NOT LIKE '/category/%'
        AND PageUrl NOT LIKE '/index_.html'
        AND PageUrl<>'/index.html'
        AND PageUrl NOT LIKE '/pages/%'
        AND PageUrl<>'/search.html'
        AND PageUrl<>'/test.html'
    GROUP BY
        PageUrl)
    SELECT
        ',["' + PageUrl + '",' + CAST(Pageviews AS VARCHAR(10)) + ']'
    FROM
        refined_summary 
    ORDER BY 
        PageUrl ;

## 2. Create JavaScript file with pageviews

The file name might be blog_metrics_universal_analytics.js. The content of the file is.

    :::javascript
    const blog_views_universal = new Map([
    ["/access-minio-s3-storage-prestodb-cluster-hive-metastore.html",4049]
    ,["/access-minio-s3-storage-prestodb-cluster.html",1627]
    ,["/access-minio-secured-ssl-aws-python-sdk.html",1440]
    ]);

## 3. Develop JavaScript code to extract pageviews

The file name might be pageviews.js. It will be embedded in each of your Web pages.

    :::javascript
    window.addEventListener('DOMContentLoaded', function () {
	pagePath = window.location.pathname;
	
	pageViewsAnalyticsUniversal = 0;
	
	if (blog_views_universal.get(pagePath)) {
		pageViewsAnalyticsUniversal = blog_views_universal.get(pagePath); 
	}
	
	
	document.getElementById('query-output').value = pageViewsAnalyticsUniversal;
	
    });

## 4. Create a HTML file to show pageviews

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

    <script src='blog_metrics_universal_analytics.js'></script>

    <script src='pageviews.js'></script>

    </body>
    </html>

## 5. Improvements

You can combine Google Universal Analytics with Google Analytics 4. The sample is [Add Google Analytics 4 Pageviews in Static Web Site]({filename}/articles/add-google-analytics-4-pageviews-static-web-site.md).
