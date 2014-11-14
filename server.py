# -*- coding: utf-8 -*-
__author__ = 'pavel'

import os
import re
import time
import csv
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

RUNS = "runs.txt"
MASTER_FILE = os.path.join(".git", os.path.join("refs", os.path.join("heads", "master")))

PATTERN = re.compile("^[B-G][^\\d]*(" + "\\s".join("\\d+(?:(?:\\.|\\,)\\d+)?" for _ in range(10)) + ").*")

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
        if os.path.exists(RUNS):
            with open(RUNS, "rt") as fd:
                runs = int(fd.readline())
        else:
            runs = 0
        suffix = ""
        if (runs % 100 < 10 or runs % 100 > 20) and (runs % 10) in [2, 3, 4]:
            suffix = "а"
        self.render("index.html", runs=runs, suffix=suffix, update=str(time.ctime(os.path.getmtime(MASTER_FILE))))


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
        print("Hey-hey-hey! New data arived!")
        zip_file = self.request.files['zip_file'][0]
        original_fname = zip_file['filename']
        if original_fname.endswith(".zip"):
            print("Yeah! Data is zip.")
            updateruns()
            final_filename = "%s-%s" % \
                             (''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(6)),
                              original_fname)

            output_file = open(os.path.join(UPLOADS, final_filename), 'wb')
            output_file.write(zip_file['body'])
            output_file.close()

            print("Let our monkeys play with data (%s)." % final_filename)
            try:
                resultsZip = monkeyfunction(final_filename)
                print("Monkeys are tired, let us send the respond.")
                self.finish('{"status" : "ok", "filename" : "%s"}' % resultsZip)
            except:
                self.finish('{"status" : "error", "reason" : "Rscript error"}')
        else:
            print("Data is bad :(")
            self.finish('{"status" : "error", "reason" : "file format"}')


def read_config(config_path):
    config = []
    with open(config_path) as config_file:
        config_reader = csv.reader(config_file, delimiter=';')
        for i, row in enumerate(config_reader):
            cnc = list()
            for c in row[1:]:
                cnc.append(float(c))
            config.append((row[0], cnc))
    return config


def read_data_from(raw_path, config):
    cocoa = {}
    cnc = sorted(config[0][1])
    cocoa["dose"] = cnc
    cocoa["name"] = os.path.basename(raw_path)
    # concentration low -> high
    with open(raw_path, "rt") as fd:
        i = 0
        for line in fd:
            line = line
            eline = "\t".join(line.strip().split()[:12])
            result = PATTERN.search(eline)
            if result and i < 6:
                data = result.group(1).split()
                for p, d in enumerate(data):
                    data[p] = float(d)
                if config[i][0] not in cocoa.keys():
                    cocoa[config[i][0]] = list()
                corder = list()
                for apple in config[i][1]:
                    corder.append(cnc.index(apple))
                data = [data[ci] for ci in corder]
                cocoa[config[i][0]].append(data)
                i += 1
    return cocoa


def write_data_to(data, file_path):
    with open(file_path, 'wb') as fp:
        writer = csv.writer(fp, delimiter=";")
        for ds in data:
            writer.writerow(["sample", ds["name"]])
            writer.writerow([""] + ds["dose"])
            ds.pop("name")
            ds.pop("dose")
            # write sample after sample
            for sample, values in ds.iteritems():
                for row in values:
                    writer.writerow([sample] + row)
        fp.close()
    return


def monkeyfunction(fname):
    zipfile_path = os.path.join(UPLOADS, fname)
    basename = os.path.splitext(fname)[0]
    unzip_dir = os.path.join(UPLOADS, basename)
    results_dir = os.path.join(RESULTS, basename)
    os.mkdir(results_dir)

    inzip = zipfile.ZipFile(zipfile_path, 'r')
    inzip.extractall(unzip_dir)

    config_path = os.path.join(unzip_dir, "config.csv")
    config = read_config(config_path)

    data = []
    for f in os.listdir(unzip_dir):
        raw_path = os.path.join(unzip_dir, f)
        if raw_path != config_path:
            data.append(read_data_from(raw_path, config))

    file_path = os.path.join(results_dir, "%s.csv" % basename)
    write_data_to(data, file_path)

    # Your logic here
    command = "Rscript bioassay-roller.R %s %s/" % (os.path.abspath(file_path), os.path.abspath(results_dir))
    print(command)
    try:
        lines = run(cmd(command))
        for _ in lines:
            pass
    except:
        print("Rscript internal error")

    results_zip = "%s.zip" % basename
    curdir = os.path.abspath(os.path.curdir)
    os.chdir(RESULTS)
    zipf = zipfile.ZipFile(results_zip, 'w')
    zipdir(basename + "/", zipf)
    zipf.close()
    os.chdir(curdir)
    return os.path.join(RESULTS, results_zip)


def updateruns():
    if os.path.exists(RUNS):
        with open(RUNS, "rt") as fd:
            runs = int(fd.readline())
    else:
        runs = 1
    with open(RUNS, "wt") as fd:
        fd.write("%s\n" % (runs + 1))


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