# Markdown Book renderer and HTTP service

This is work in progress, can be still instable. It serves the markdown book on http port 80.

## Basic usage

Create the folder with your markdown book (see folder structure below), and create there following `dockerfile`

```dockerfile
FROM milung/render-md
ADD . /var/www/book
```

then run it to see your book rendered: 

```sh
docker run -p 4320:80 --name my-md-book
```

and navigate to the [http://localhost:4200]. Alternatively, you can run it with local 
folder mounted to `/var/www/book` during book authoring: 

```sh
docker run -p 4320:80 -v ${PWD}:/var/www/book --name my-md-book
```

Currently there is no automatic refresh durring authoring. 

## Book folder structure

The following is recommended book folder structure:

```
<project root>
 │
 ├──book-src
 │  │
 │  ├──<chapter-XX>
 │  │  │
 │  │  ├─── README.md
 │  │  └─── <chapter files and subchapter folders>
 │  │ 
 │  ├─── README.md
 │  ├─── SUMMARY.md
 │  └─── LINKS.md
 │
 └─── book.json
 ```

 The file `book.json` is the configuration manifest of your book. The manifest is optional, if not 
 present then default settings are assumed for all of its parameters. The following parameters 
 can be configured in the manifest: 

* **source-root** (default: "."): The folder where to look for the book files. 
    The    URL paths are resolved relatively to this root. 
* **title** (default: "Book"): The title of your book.
* **index** (default: "README.md"): name of the file in the chapter directory, 
    which is used if the URL path resolves to a directory. 
* **summary** (default: "SUMMARY.md"): The file rendered on the side pane as a navigation. Typically contains links to all your chapters and sections. Side pane is not rendered if this document does not exists. The path is relative to the `source-root` folder.
* **links** (default: "LINKS.md"): Optional auxiarly file used to specify link reference definitions for your book. This file is loaded with each of your section. The links shall be provided in the format `[foo]: /url "title"`, each on separate line. The link reference definitions are not rendered into the result document. The path is relative to the `source-root` folder.


## Environment variables

The following environment variables control the behavior of the server: 

 * **http_port** (default: 80) - port number on which the http server will listen.
 * **base_url** (default: /) - URL path on which the server is serving the book. Used to prefix absolute links with this path in the case the server is behind reverse-proxy. 
 * **book_base** (default: /var/www/book) - the root folder of your book project - the one where the server is looking for the `book.json` configuration manifest. All your book markdown files and assets must be placed under this folder. 