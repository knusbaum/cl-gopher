# cl-gopher

cl-gopher is a Common Lisp library for interacting with the [Gopher](https://en.wikipedia.org/wiki/Gopher_(protocol)) protocol.
It is suitable for building both clients and servers, and provides a sample client.

## Basic Usage

Note: This documentation assumes you know a bit about the gopher protocol, and doesn't cover
the protocol's basic workings. 

### Gopher Lines
The main class this library deals with is the `GOPHER-LINE`.
A `GOPHER-LINE` represents a gopher menu item, (analogous to a html link)

`GOPHER-LINE` has a number of subclasses, depending on the type of the gopher link.

Lines can be created directly with `MAKE-INSTANCE`, by parsing gopher URIs with `PARSE-GOPHER-URI`, by reading lines from a stream (e.g. to a server speaking the gopher protocol) with `READ-GOPHER-LINE`, or by converting from an alist with `GOPHER-LINE-FROM-ALIST`.

Once you have a line, you have the ability to:
* Retrieve the content it links to with `GET-LINE-CONTENTS` (contents explained more below)
* Write the line out to a stream according to Gopher protocol with `WRITE-GOPHER-LINE`
* Convert the line to and from an alist format with `GOPHER-LINE-TO-ALIST`, `GOPHER-LINE-FROM-ALIST`, `GOPHER-LINES-TO-ALIST` and `GOPHER-LINES-FROM-ALIST`. This is useful if you want to `READ` lines from a file, or `WRITE` lines to a file.
* Display the line in a human-readable way with `DISPLAY-LINE` or `DISPLAY-LINES`
* Download the raw content bytes of a link's target to a file with `DOWNLOAD-FILE`

There are several other minor util functions that accept a `GOPHER-LINE` as an argument.

### Contents
The other major class family in the library is the `SELECTOR-CONTENTS` class and subclasses.
Instances of these classes are returned from `GET-LINE-CONTENTS`, and represent the various kinds of content that Gopher URIs can link to.
The classes currently are:
* `SUBMENU-CONTENTS` - Contents of a Gopher menu. Its one slot, `lines`, is a list of `GOPHER-LINE` contained in the menu.
* `TEXT-FILE-CONTENTS` - Contents of a text file. Its one slot, `lines`, contains a list of human-readable text lines from the text document.
* `HTML-FILE-CONTENTS` - Contains the content of an html page as a single string in slot `content-string`
* `BINARY-FILE-CONTENTS` - Contains the binary contents of a file such as an image or executable in the slot `content-array`, and the name of the file in `file-name`

All that is provided by the for working with contents objects are the slot accessors, and a generic function, `DISPLAY-CONTENTS`, which displays a contents object in human-readable format.

### Quick Examples:

cl-gopher includes a simple text browser client to exemplify the use of the library. You can test it with:
```
CL-USER> (cl-gopher:text-browser)
	Welcome to the SDF Public Access UNIX System .. est. 1987
	
	Official Site of the Internet Gopher Club Underground Syndicate
	
	We offer FREE and inexpensive memberships for people interested
	in the UNIX system and internetworking.  Personal GOPHERSPACE
	is available to all users as well as hundreds of UNIX utilities,
	games and networking utilities.  We are a federally recognized
	non-profit 501(c)7 organization and we are supported entirely
	by donations and membership dues.  ssh://sdf.org
	
11     SUBMENU SDF PHLOGOSPHERE (226 phlogs)
12     SUBMENU SDF GOPHERSPACE (1119 ACTIVE users)
13     SUBMENU SDF GOPHERSPACE (1371 AGED users)
14     SUBMENU SDF GOPHERSPACE (373 ANCIENT users)
15     SUBMENU SDF Frequently Asked Questions (FAQ)
16     SUBMENU SDF Accredited University Courses
17     SUBMENU Software and Documentation for various computers
18     SEARCH-LINE GopherSpace SEARCH Engine
19     SUBMENU Floodgap's GOPHERSPACE
	____________________________________________________________________________
	                        Gophered by Gophernicus/97 on NetBSD/amd64 8.0_BETA
Select a line number, or "help".
> 
```

Example showing how to get and display the contents of a gopher uri:
```
CL-USER> (cl-gopher:display-contents
           (cl-gopher:get-line-contents
             (cl-gopher:parse-gopher-uri "sdf.org")))
    Welcome to the SDF Public Access UNIX System .. est. 1987
	
	Official Site of the Internet Gopher Club Underground Syndicate
	
	We offer FREE and inexpensive memberships for people interested
	in the UNIX system and internetworking.  Personal GOPHERSPACE
	is available to all users as well as hundreds of UNIX utilities,
	games and networking utilities.  We are a federally recognized
	non-profit 501(c)7 organization and we are supported entirely
	by donations and membership dues.  ssh://sdf.org
	
11     SUBMENU SDF PHLOGOSPHERE (226 phlogs)
12     SUBMENU SDF GOPHERSPACE (1119 ACTIVE users)
13     SUBMENU SDF GOPHERSPACE (1371 AGED users)
14     SUBMENU SDF GOPHERSPACE (373 ANCIENT users)
15     SUBMENU SDF Frequently Asked Questions (FAQ)
16     SUBMENU SDF Accredited University Courses
17     SUBMENU Software and Documentation for various computers
18     SEARCH-LINE GopherSpace SEARCH Engine
19     SUBMENU Floodgap's GOPHERSPACE
	____________________________________________________________________________
	                        Gophered by Gophernicus/97 on NetBSD/amd64 8.0_BETA

```

A very basic example of how a server could use the library. Assume remote-sock is a socket connected to a client:
```
CL-USER> (let ((lines (list
                       (make-instance 'cl-gopher:text-file
                                      :hostname "myserver.org"
                                      :port 70
                                      :selector "/Welcome.txt"
                                      :display-string "Welcome to myserver.org!")
                       (make-instance 'cl-gopher:submenu
                                      :hostname "myserver.org"
                                      :port 70
                                      :selector "/Documents"
                                      :display-string "View myserver.org's documents."))))
           (mapcar (lambda (gl) (cl-gopher:write-gopher-line gl remote-sock)) lines))
```
Output sent to remote:
```
0Welcome to myserver.org!	/Welcome.txt	myserver.org	70
1View myserver.org's documents.	/Documents	myserver.org	70

```
