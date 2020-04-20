Title: Create OAuth Credentials for Google Analytics APIs
Date: 2020-04-18
Category: Google
Cover: /extra/google-analytics-logo.png

One of the ways to implement Google Analytics in your tools or Web sites is to utilize Google Analytics APIs. This way is very flexible as Google Analytics APIs can be used with wide range of the programming languages. Moreover, APIs are mature product which on the market for many years. The latest v4 contains rich set of functionalities which was forged from earliest versions. It's back compatible with previous v3. The first step to start using Google Analytics APIs includes creating of an OAuth credentials. There are many options to proceed with it but we follow a route to generate credentials for a Web site with javascript implementation. It's applicable to other scenarios as well.

## 1. Create a Google account

All Google tools request a Google account. If you already have a Google email, it can be used as your Google account otherwise follow [link](https://accounts.google.com/signup).

![Create Google Account]({static}/images/create-oauth-credentials-for-google-analytics-apis/create-google-account.png)</br></br>

## 2. Open Google Developer Console

[Google Developer Console](https://console.developers.google.com) is used to create an OAuth credentials. To open the tool, sign in with your Google account, and then agree to Terms of Service.

![Google Developer Console Landing Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-developer-console-landing-screen.png)</br></br>

## 3. Create a new API project

* Type in your project name. If you create a new project for your personal usage, leave **Location** with **No organization** default value.

![Create Google API Project]({static}/images/create-oauth-credentials-for-google-analytics-apis/create-google-api-project.png)</br></br>

* Your project name is the current one.

![Google API and Services Dashboard]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-apis-and-services-dashboard.png)</br></br>

## 4. Enable Google Analytics APIs

* Click **ENABLE APIS AND SERVICES** button.
![Enable Google Analytics APIs]({static}/images/create-oauth-credentials-for-google-analytics-apis/enable-google-analytics-apis.png)</br></br>

* Search for **Google Analytics APIs** from the list of available APIs.
![Search Google Analytics APIs]({static}/images/create-oauth-credentials-for-google-analytics-apis/search-google-analytics-apis.png)</br></br>

* Confirm your intention.
![Confirm Google Analytics APIs Enabling]({static}/images/create-oauth-credentials-for-google-analytics-apis/confirm-google-analytics-apis-enabling.png)</br></br>

## 5. Configure Consent

* Select Credentials menu item from the dashboard.
![Select Google Credentials Landing Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/select-google-credentials-landing-screen.png)</br></br>

* Initiate configuration of consent clicking **CONFIGURE CONSENT SCREEN**.
![Configure Google Consent Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/configure-google-consent-screen.png)</br></br>

* If you left **Location** with **No organization** value, you have only **External** User Type option enabled.
![Google OAuth Consent Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-consent-screen.png)</br></br>

* Enter information about your Web site.
![Google OAuth Consent Form]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oath-consent-form.png)</br></br>

* Final screen of the configuration.
![Google OAuth Consent Final Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-consent-final-screen.png)</br></br>

## 6. Create an OAuth credentials

* Go back to Credentials screen and click **CREATE CREDENTIALS** button, and then select OAuth client ID.
![Google OAuth Credentials]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-create-oauth-credentials.png)</br></br>

* Select **Web application** type, type in your application name, and enter your website. If you test it, you can enter http://localhost.com.
![Google OAuth Credentials Form]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-create-oauth-credentials-form.png)</br></br>

* Confirmation screen.
![Create Google OAuth Credentials Confirmation Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/create-google-oauth-credentials-confirmation-screen.png)</br></br>

* Your OAuth credentials created.
![Google Credentials Screen with OAuth Created]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-credentials-screen-with-oauth-created.png)</br></br>

* OAuth credentials information can be retrieved if you click on the name of **OAuth 2.0 Client** ID entry.
![Google OAuth Credentials Information]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-credentials-information.png)</br></br>

* Clicking on **DOWNLOAD JSON** downloads your credentials as json file.

        :::json
        {"web":
               {"client_id":"109743573222-tu7960r1m6kam5acmigfumlqebf016cf.apps.googleusercontent.com",
                "project_id":"tech-jogging-blog",
                "auth_uri":"https://accounts.google.com/o/oauth2/auth",
                "token_uri":"https://oauth2.googleapis.com/token",
                "auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs",
                "client_secret":"kEbM6uLv3NsgeEpiLDVz8hxS",
                "javascript_origins":["https://techjogging.com"]
               }
        }

## 7. Sample API v3

[The sample](https://developers.google.com/analytics/devguides/reporting/core/v3/quickstart/web-js) shows a number of sessions for the last 30 days.

Create an HTML file on your server replacing CLIENT_ID with yours in`CLIENT_ID = '109743573222-tu7960r1m6kam5acmigfumlqebf016cf.apps.googleusercontent.com'` line, and open it in browser. It will ask for your Google account credentials.

    :::html
        <!DOCTYPE html>
    <html>
    <head>
      <meta charset="utf-8">
      <title>Hello Analytics - A quickstart guide for JavaScript</title>
    </head>
    <body>

    <button id="auth-button" hidden>Authorize</button>

    <h1>Hello Analytics</h1>

    <textarea cols="80" rows="20" id="query-output"></textarea>

    <script>

      // Replace with your client ID from the developer console.
      var CLIENT_ID = '109743573222-tu7960r1m6kam5acmigfumlqebf016cf.apps.googleusercontent.com';

      // Set authorized scope.
      var SCOPES = ['https://www.googleapis.com/auth/analytics.readonly'];


      function authorize(event) {
        // Handles the authorization flow.
        // `immediate` should be false when invoked from the button click.
        var useImmdiate = event ? false : true;
        var authData = {
          client_id: CLIENT_ID,
          scope: SCOPES,
          immediate: useImmdiate
        };

        gapi.auth.authorize(authData, function(response) {
          var authButton = document.getElementById('auth-button');
          if (response.error) {
            authButton.hidden = false;
          }
          else {
            authButton.hidden = true;
            queryAccounts();
          }
        });
      }


    function queryAccounts() {
      // Load the Google Analytics client library.
      gapi.client.load('analytics', 'v3').then(function() {

        // Get a list of all Google Analytics accounts for this user
        gapi.client.analytics.management.accounts.list().then(handleAccounts);
      });
    }


    function handleAccounts(response) {
      // Handles the response from the accounts list method.
      if (response.result.items && response.result.items.length) {
        // Get the first Google Analytics account.
        var firstAccountId = response.result.items[0].id;

        // Query for properties.
        queryProperties(firstAccountId);
      } else {
        console.log('No accounts found for this user.');
      }
    }


    function queryProperties(accountId) {
      // Get a list of all the properties for the account.
      gapi.client.analytics.management.webproperties.list(
          {'accountId': accountId})
        .then(handleProperties)
        .then(null, function(err) {
          // Log any errors.
          console.log(err);
      });
    }


    function handleProperties(response) {
      // Handles the response from the webproperties list method.
      if (response.result.items && response.result.items.length) {

        // Get the first Google Analytics account
        var firstAccountId = response.result.items[0].accountId;

        // Get the first property ID
        var firstPropertyId = response.result.items[0].id;

        // Query for Views (Profiles).
        queryProfiles(firstAccountId, firstPropertyId);
      } else {
        console.log('No properties found for this user.');
      }
    }


    function queryProfiles(accountId, propertyId) {
      // Get a list of all Views (Profiles) for the first property
      // of the first Account.
      gapi.client.analytics.management.profiles.list({
          'accountId': accountId,
          'webPropertyId': propertyId
      })
      .then(handleProfiles)
      .then(null, function(err) {
          // Log any errors.
          console.log(err);
      });
    }


    function handleProfiles(response) {
      // Handles the response from the profiles list method.
      if (response.result.items && response.result.items.length) {
        // Get the first View (Profile) ID.
        var firstProfileId = response.result.items[0].id;

        // Query the Core Reporting API.
        queryCoreReportingApi(firstProfileId);
      } else {
        console.log('No views (profiles) found for this user.');
      }
    }


    function queryCoreReportingApi(profileId) {
      // Query the Core Reporting API for the number sessions for
      // the past seven days.
      gapi.client.analytics.data.ga.get({
        'ids': 'ga:' + profileId,
        'start-date': '30daysAgo',
        'end-date': 'yesterday',
        'metrics': 'ga:sessions'
      })
      .then(function(response) {
        var formattedJson = JSON.stringify(response.result, null, 2);
        document.getElementById('query-output').value = formattedJson;
      })
      .then(null, function(err) {
          // Log any errors.
          console.log(err);
      });
    }

      // Add an event listener to the 'auth-button'.
      document.getElementById('auth-button').addEventListener('click', authorize);
    </script>

    <script src="https://apis.google.com/js/client.js?onload=authorize"></script>

    </body>
    </html>

Response is

    :::json
    {
      "kind": "analytics#gaData",
      "id": "https://www.googleapis.com/analytics/v3/data/ga?ids=ga:209816969&metrics=ga:sessions&start-date=30daysAgo&end-date=yesterday",
      "query": {
        "start-date": "30daysAgo",
        "end-date": "yesterday",
        "ids": "ga:209816969",
        "metrics": [
          "ga:sessions"
        ],
        "start-index": 1,
        "max-results": 1000
      },
      "itemsPerPage": 1000,
      "totalResults": 1,
      "selfLink": "https://www.googleapis.com/analytics/v3/data/ga?ids=ga:209816969&metrics=ga:sessions&start-date=30daysAgo&end-date=yesterday",
      "profileInfo": {
        "profileId": "209816969",
        "accountId": "156524189",
        "webPropertyId": "UA-156524189-1",
        "internalWebPropertyId": "220549817",
        "profileName": "All Web Site Data",
        "tableId": "ga:209816969"
      },
      "containsSampledData": false,
      "columnHeaders": [
        {
          "name": "ga:sessions",
          "columnType": "METRIC",
          "dataType": "INTEGER"
        }
      ],
      "totalsForAllResults": {
        "ga:sessions": "94"
      },
      "rows": [
        [
          "94"
        ]
      ]
    }
