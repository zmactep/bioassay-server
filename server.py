# -*- coding: utf-8 -*-
__author__ = 'pavel'

import os
import random
import string
import zipfile
import shutil
from iterpipes import cmd, run

import tornado.httpserver
import tornado.ioloop
import tornado.options
import tornado.web

from tornado.options import define, options

define("port", default=1331, help="run on the given port", type=int)

UPLOADS = "uploads"
RESULTS = "results"


class Application(tornado.web.Application):
    def __init__(self):
        settings = {
            'debug': True,
            'static_path': 'static'
        }

        handlers = [
            (r"/", IndexHandler),
            (r"/upload", UploadHandler),
            (r"/results", DownloadHandler),
            (r"/.*", Error404)
        ]
        tornado.web.Application.__init__(self, handlers, **settings)


class IndexHandler(tornado.web.RequestHandler):
    def get(self):
        self.render("index.html")


class Error404(tornado.web.RequestHandler):
    def get(self):
        self.render("404.html")


class DownloadHandler(tornado.web.RequestHandler):
    def get(self):
        zipfile = self.get_argument("zipfile", default=None, strip=True)
        if zipfile and os.path.exists(zipfile):
            sendfile(self, zipfile)
            self.finish()
        else:
            self.finish("No such file :(")


class UploadHandler(tornado.web.RequestHandler):
    def post(self):
        csv_file = self.request.files['csv_file'][0]
        original_fname = csv_file['filename']
        if original_fname.endswith(".csv"):
            fname = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(6))
            final_filename = "%s-%s" % (fname, original_fname)
            output_file = open(os.path.join(UPLOADS, final_filename), 'w')
            output_file.write(csv_file['body'])
            output_file.close()
            resultsZip = monkeyfunction(final_filename)
            self.finish('{"status" : "ok", "filename" : "%s"}' % resultsZip)
        else:
            self.finish('{"status" : "error", "reason" : "file format"}')


def monkeyfunction(fname):
    file_path = os.path.join(UPLOADS, fname)
    basename = os.path.splitext(fname)[0]
    results_dir = os.path.join(RESULTS, basename)
    os.mkdir(results_dir)

    # Your logic here
    command = "Rscript bioassay-roller.R %s %s/" % (os.path.abspath(file_path), os.path.abspath(results_dir))
    print(command)
    try:
        run(cmd(command))
    except:
        print("Rscript internal error")

    # Copy input data too
    shutil.copy(file_path, os.path.join(results_dir, os.path.basename(file_path)))

    results_zip = "%s.zip" % basename
    curdir = os.path.abspath(os.path.curdir)
    os.chdir(RESULTS)
    zipf = zipfile.ZipFile(results_zip, 'w')
    zipdir(basename + "/", zipf)
    zipf.close()
    os.chdir(curdir)
    return os.path.join(RESULTS, results_zip)


def sendfile(self, file_name):
    buf_size = 4096
    self.set_header('Content-Type', 'application/octet-stream')
    self.set_header('Content-Disposition', 'attachment; filename=' + os.path.basename(file_name))
    with open(file_name, 'rb') as f:
        while True:
            data = f.read(buf_size)
            if not data:
                break
            self.write(data)


def result(file_name):
    return os.path.join(RESULTS, file_name)


def zipdir(path, zip):
    for root, _, files in os.walk(path):
        for f in files:
            zip.write(os.path.join(root, f))


def main():
    http_server = tornado.httpserver.HTTPServer(Application())
    http_server.listen(options.port)
    tornado.ioloop.IOLoop.instance().start()


if __name__ == "__main__":
    main()