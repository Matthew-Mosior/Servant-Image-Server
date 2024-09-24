# Servant-Image-Server

## Introduction
Servant-Image-Server (**SIS**) is a tool for labeling image data via object detection.

## Implementation
Below are a some of the notable libraries used to implement **SIS**:
- servant libraries to create the web API/Server/Client.
- aeson for parsing JSON returned from API calls.
- sqlite-simple for all things persistence via sqlite3.
- wai/warp for actually running the webserver.
- hspec for testing.

## Persistence
This tool utilizes sqlite3 in order to persistently store image and related data.

You will need access to sqlite3 in order for this tool to work.

## Imagga
This tool is designed around interacting with *Imagga*, an online object detection API.  As a result, a custom JSON parser was implemented around the expected JSON response format returned from calls to *Imagga*, and custom http requests were created in order to query its API.

In order to interact with *Imagga*'s object detection API, you need to create an account.  Once you do so, you will be supplied with your very own *api key* and *api secret*.

### Note
This tools works with image URLs only.

## Example Usage
**SIS** is easy to use.

Below is how to start up **SIS**:

`$ cd server`

`$ stack build`

`$ stack exec server-exe [api-key] [api-secret]`

Once the server has been started, you can begin to play!

In another terminal, you could do the following:

`$ curl -X POST -d '{"imageinput_url":"https://docs.imagga.com/static/images/docs/sample/japan-605234_1280.jpg", "imageinput_label": "japan", "imageinput_enableobjectdetection": true}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/images`
 - You can optionally specify `"imageinput_enableobjectdetection": false` should you want to skip querying and labeling via *Imagga*'s API and just stored it in the backend.
 - Additionally, you can omit the `"imageinput_label"` key and the server will label the image with the identifier that is generated for it.

This will query the API with the image found at `https://docs.imagga.com/static/images/docs/sample/japan-605234_1280.jpg`.

The response received will look something like below:

`{"imageinput'_data":"...","imageinput'_enableobjectdetection":true,"imageinput'_identifier":10,"imageinput'_label":"japan","imageinput'_responsestatuscode":200,"imageinput'_url":"https://docs.imagga.com/static/images/docs/sample/japan-605234_1280.jpg"}`

This will result in the server doing a few things:
 - Downloading and decoding the image file in order to return the raw image data.
 - Creating a sqlite identifier.
 - Creating a label (if the user did not provide one).
 - Inserting the identifier, label, and url into sqlite.
 - Inserting rows for all labels (in another table different from the one above) returned from *Imagga* with a 50% confidence or greater.
 - Returning the above plus the raw data and a boolean describing whether or not object detection was requested on the image.

You can also query the backend via a couple of different endpoints:
- `$ curl http://localhost:8080/images`
  -   Returns metadata for all images stored in the backend.
- `$ curl http://localhost:8080/images$objects="querystring"`
  - The `querystring` has the format `"string1,string2,...,stringn"`.
  - Returns image metadata for all images that contain labels as identified by *Imagga*. 
- `$ curl http://localhost:8080/images/id`
  - Returns image metadata for the image stored at the `id`.
  - `imageinput'_identifier` in the response is the `id` to plug in here.

## Test Suite
A simple test suite was implemented to ensure accuracy to the API specification.

You can run the test suite with the following commands:

`$ cd server`

`$ stack test --test-arguments='[api-key] [api-secret]'`
