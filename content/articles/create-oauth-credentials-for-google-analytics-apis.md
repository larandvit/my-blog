Title: Create OAuth Credentials for Google Analytics APIs
Date: 2020-04-18
Category: Google
Cover: /extra/google-analytics-logo.png

One of the ways to implement Google Analytics in your tools or Web sites is to utilize Google Analytics APIs. This way is very flexible as Google Analytics APIs can be used with wide range of the programming languages. Moreover, APIs are mature product which on the market for many years. The latest v4 contains rich set of functionalities which was forged from earliest versions. It's back compatible with previous v3. The first step to start using Google Analytics APIs includes creating of an OAuth credentials. There are many options to proceed with it but we follow a route to generate credentials for a Web site with javascript implementation. It's scenarios to other scinerios as well.

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

* Click **ENABLES APIS AND SERVICES** button
![Enable Google Analytics APIs]({static}/images/create-oauth-credentials-for-google-analytics-apis/enable-google-analytics-apis.png)</br></br>

* Search for **Google Analytics APIs** from the list of available APIs
![Search Google Analytics APIs]({static}/images/create-oauth-credentials-for-google-analytics-apis/search-google-analytics-apis.png)</br></br>

* Confirm your intention
![Confirm Google Analytics APIs Enabling]({static}/images/create-oauth-credentials-for-google-analytics-apis/confirm-google-analytics-apis-enabling.png)</br></br>

## 5. Configure Consent

* Select Credentials menu item from the dashboard
![Select Google Credentials Landing Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/select-google-credentials-landing-screen.png)</br></br>

* Initiate configuration of consent clicking **CONFIGURE CONSENT SCREEN**
![Configure Google Consent Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/configure-google-consent-screen.png)</br></br>

* If you left **Location** with **No organization** value, you have only **External** User Type option enabled.
![Google OAuth Consent Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-consent-screen.png)</br></br>

* Enter information about your Web site.
![Google OAuth Consent Form]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oath-consent-form.png)</br></br>

* Final screen of the configuration.
![Google OAuth Consent Final Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-consent-final-screen.png)</br></br>

## 6. Create an OAuth credentials

* Go back to Credentials screen and click **CREATE CREDENTIALS** button, and then select OAuth client ID
![Google OAuth Credentials]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-create-oauth-credentials.png)</br></br>

* Select **Web application** type, type in your application name, and enter your website. If you test it, you can enter http://localhost.com
![Google OAuth Credentials Form]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-create-oauth-credentials-form.png)</br></br>

* Confirmation screen
![Create Google OAuth Credentials Confirmation Screen]({static}/images/create-oauth-credentials-for-google-analytics-apis/create-google-oauth-credentials-confirmation-screen.png)</br></br>

* Your OAuth credentials created
![Google Credentials Screen with OAuth Created]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-credentials-screen-with-oauth-created.png)</br></br>

* OAuth credentials information can be retrieved if you click on the name of **OAuth 2.0 Client** ID entry
![Google OAuth Credentials Information]({static}/images/create-oauth-credentials-for-google-analytics-apis/google-oauth-credentials-information.png)</br></br>
