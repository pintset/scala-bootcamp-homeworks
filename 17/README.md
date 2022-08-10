## 17.  Asynchronous Programming - JVM threads, Futures & Promises, synchronized, Atomic*

Application:

- takes a web-page URL from arguments (args array)

- loads the web-page body, extracts HTTP links from it

- for all the found links, tries to fetch a server name header if there is one

- prints all the encountered unique server name values in alphabetical order


Each link processing should be done in parallel. Validation of arguments is not needed.

Try to test it on http://google.com!