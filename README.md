# Peytonfarrar-www
The source for the website peytonfarrar.com built using caveman2

## Installation
1. Install SBCL preferably with threading support
2. Install [Quicklisp](https://www.quicklisp.org/beta/)
3. Clone the repository

## Running
Firstly navigate to the peytonfarrar-www directory, then execute the command
`sbcl --load peytonwww.asd`. You should now be in the SBCL prompt. Run the
command `(ql:quickload :peytonwww)`, this will install the required
dependencies and load the project. You can now start the server with
`(peytonwww:start :port 8000)`, and you will be able to see the webpage at
localhost:8000. You can stop the server at time from the SBCL prompt by
running `(peytonwww:stop)`.

## Deployment
I am currently using NGINX to deploy the server. This can be done by using
NGINX as a reverse proxy for the web server. The configuration for this would
look something like: 
```
http {
	server { 
		listen       80;                                                                                                      
		location / {                                                                                                          
			proxy_pass http://0.0.0.0:8000;
			charset utf-8;                                                                                                      
 	}                                                                                                                     
}
```
