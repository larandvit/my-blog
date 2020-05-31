Title: Add Google Analytics Pageviews in Static Web Site
Date: 2020-05-31
Category: Google Analytics
Cover: /extra/google-analytics-logo.png

Google Analytics requests authentication to access Google Analytics data/reports. Implementation of Google Analytics can be done in both places client and server. Server APIs support a wide variety of languages and the authentication process is transparent for users. Javascript is a language to access Google Analytics APIs in client implementation. Also, to view Google Analytics in javascript, users have to be authenticated. The process of authentication includes a Google form to enter user credentials. It works perfectly for a set of scenarios but it doesn't work if we want to see pageviews in static web sites.

To solve an issue with authentication, we can use a Google Analytics token. A token allows bypass entering of credentials. It will add an extra step to generate a token and place it in a location accessible by your static web site. Google Analytics token has limited life; it is only 1 hour, so we need to renew our token every hour. It seems complicated but in fact not. It can be used a tool to generate tokens and push it in a static web site. 

A sample is located in [github](https://github.com/larandvit/google-analytics-pageview-counter) repository. Static web site implementation is [Pelican blog](https://techjogging.com/redirect-http-to-https-in-synology-nas-nginx.html).

## 1. Create Google Analytics service account and extract json key file

* Go to [Google Analytics Platform APIs and Services Credentials](https://console.cloud.google.com/apis/credentials) to create a Service account

![Google Cloud Platform APIs Services Credentials]({static}/images/add-google-analytics-pageviews-static-web-site/google-cloud-platform-apis-services-credentials.png)</br></br>

* Create Key

![Google Analytics Service Account Generate Key]({static}/images/add-google-analytics-pageviews-static-web-site/google-analytics-service-account-generate-key.png)</br></br>

## 2. Develop Generate Google Analytics token tool

The tool will use `oauth2client` library to run `from_json_keyfile_name` function from `ServiceAccountCredentials` service object. After getting a token, the token file will be updated on the static web site.

    :::python
    from oauth2client.service_account import ServiceAccountCredentials
    
    # The scope for the OAuth2 request.
    SCOPE = 'https://www.googleapis.com/auth/analytics.readonly'

    # Defines a method to get an access token from the ServiceAccount object.
    def access_token(key_file_path):
        return ServiceAccountCredentials.from_json_keyfile_name(key_file_path, SCOPE).get_access_token()

    if __name__ == "__main__":

        key_file_path = 'tech-jogging-blog-98stj21aac52.json'
        
        token_file_path = 'report_access.js'

        with open(token_file_path, "w") as f_out:
            token = access_token(key_file_path).access_token
            token_code = 'var ANALYTICS_TOKEN = \'{}\';'.format(token)
            f_out.write(token_code)

Generated token javascript file looks like. It contains a variable with token value.

    :::python
    var ANALYTICS_TOKEN = 'ya29.c.Ko4BzQdIsUMOFvVHh5O90tnJZvW3but0Ym-C9C1NhKEt3ihBKEk6AOQFp-Mm-MUzZiyLJsfNSD90vqeUm078fSeXl0NXUhWZKvY79BJhg33UB_crRmwDY3Xn98KaPTgi22y4_QFdRA0l3GiQeISkQcnEmb0P1Y_eCquWR-qtDWVy-IBDZRJph2j6otc64oxoqQ';

## 3. Develop javascript code to access Google Analytics

The code runs when Google Analytics APIs is loaded, then it is used the generated token to authorize access to Google Analytics, after a pageviews query is sent, and finally the received pageview value is assigned to an HTML control.

    :::javascript
    api.analytics.ready(function() {

      /**
       * Authorize the user with an access token.
       */
      gapi.analytics.auth.authorize({
        'serverAuth': {
         'access_token': ANALYTICS_TOKEN
        }
      });
  
      var pagePathFilter = 'ga:pagePath==' + window.location.pathname;
  
      var report = new gapi.analytics.report.Data({
	  query: {
       	    ids: 'ga:209816969',
	    'start-date': 'today',
	    'end-date': 'today',
	    metrics: 'ga:pageviews',
	    filters: pagePathFilter
	  }
	});

	report.on('success', function(response) {
		document.getElementById('query-output').value = response.totalsForAllResults['ga:pageviews'];
	});

	report.execute();
  
    });

## 4. Create a HTML file to show pageviews

    :::html

    <!DOCTYPE html>
    <html>
    <head>
      <meta charset="utf-8">
      <title>Google Analytics Pageviews Sample</title>
  
    <script>
    (function(w,d,s,g,js,fs){
      g=w.gapi||(w.gapi={});g.analytics={q:[],ready:function(f){this.q.push(f);}};
      js=d.createElement(s);fs=d.getElementsByTagName(s)[0];
      js.src='https://apis.google.com/js/platform.js';
      fs.parentNode.insertBefore(js,fs);js.onload=function(){g.load('analytics');};
    }(window,document,'script'));
    </script>
 
    </head>
    <body>

    <h1>Google Analytics</h1>

    <h3>Pageviews Counter</h3>
    <textarea cols="10" rows="1" id="query-output"></textarea>

    <script src='report_access.js'></script>

    <script src='pageviews.js'></script>

    </body>
    </html>

## 5. Schedule Generate Google Analytics token tool to be run every hour

It can be done in many different ways, for example, Linux - cron, Windows - Task Scheduler, Synology DSM - Task Scheduler.
