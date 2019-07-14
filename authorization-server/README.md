# Authorization Server

An OAuth Authorization Server.

Operating on a hard coded list of clients.

## GET /authorize

The `client` should redirect the `resource owner` to the `/authorize` endpoint over `http|s` with the clients details set as `query parameters` on the `/authorize` endpoint.

Example:

A client has been hard coded within the authorization server with the following details:

```
ID: 1
Secret: 123456789
Redirect URIs: http://localhost:3000/callback
Scope: read
```

Other clients exist, for these examples we will use just this client.

Issuing a HTTP request via `curl` to the `/authorize` endpoint will validate that the request came from a known `client`. The `client` is responsible for passing details about itself via the query string like so:

```
$ curl \
"localhost:3000/authorize?client_id=1&client_secret=123456789&redirect_uri=http://localhost:3000/callback&scope=read" \
-v
```

This request results in a response of:

```
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /authorize?client_id=1&client_id=123456789&redirect_uri=http://localhost:3000/callback&scope=read HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.54.0
> Accept: */*
> 
< HTTP/1.1 302 Found
< Transfer-Encoding: chunked
< Date: Sun, 14 Jul 2019 21:08:41 GMT
< Server: Warp/3.2.28
< Location: /approve?request_id=123
< X-Forwarded-From: /authorize
< 
* Connection #0 to host localhost left intact
```

This response tells us that the authorization server acknowledged the client and redirected the `resource owner` to a new endpoint which allows the `resource owner` to approve the request for authorization delegation.

If we request authorization of a client with an invalid scope, the authorization server will issue a `404` rather than a `302`. Client with ID 1 does not have the ability to request a `write` scope.

```
$ curl \
"localhost:3000/authorize?client_id=1&client_secret=123456789&redirect_uri=http://localhost:3000/callback&scope=write" \
-v
```

This request results in a response of:

```
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /authorize?client_id=1&client_id=123456789&redirect_uri=http://localhost:3000/callback&scope=write HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.54.0
> Accept: */*
> 
< HTTP/1.1 404 Not Found
< Transfer-Encoding: chunked
< Date: Sun, 14 Jul 2019 21:13:33 GMT
< Server: Warp/3.2.28
< Content-Type: text/html; charset=utf-8
< 
* Connection #0 to host localhost left intact
Couldn't find what you were looking for.%  
```