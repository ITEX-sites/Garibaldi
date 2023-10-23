import csv
import numpy as np
from PIL import Image
import dirtools as dt
from coverage_getter import percent_cover
import datetime
import exifread


output = "../output_data/"
output_name = "final_coverage_data_{}.csv".format(datetime.date.today())
datapath = "..//raw_data//cropped_quad_photos_final_2023"

op = output + output_name

class Photo:
    def __init__(self, path, location, transect, quad, photographer):
        self.path = path
        self.data = Image.open(path)     
        self.location, self.transect = location, transect
        self.quad, self.photographer = quad, photographer
        self.value = np.nan
        self.date = None

    def return_csv_line(self):
        return (self.date, self.location, self.transect, self.quad, self.photographer,
        self.value)

    def get_date(self):
        try:
            with open(self.path, "rb") as fh:
                print(self.path)
                tags = exifread.process_file(fh, stop_tag="EXIF DateTimeOriginal")
                dateTaken = tags["EXIF DateTimeOriginal"]
                print(dateTaken)
                return dateTaken
        except Exception as err:
            print(err)
            return False


photos = []
fnames = dt.get_files(datapath)
files = dt.get_files(datapath, fullpath=True)

lastdate = "5"

for name, file in zip(fnames, files):
    name = name.replace("Copy of ", "")
    name = name.replace("-duplicate", "")
    name = name.replace(".jpeg", "")
    name = name.replace(".jpg", "")
    print(name)
    ph = Photo(file, *tuple(name.split("-")))
    date = ph.get_date()
    if date is not False:
        ph.date = date
        lastdate = date
    else:
        ph.date = lastdate

    photos.append(ph)

for photo in photos:
    print(photo.photographer)
    photo.value = percent_cover(photo.data, visualize=False)

with open(op, mode="w", newline='') as output:
    output_writer = csv.writer(
        op, delimiter=',', quoting=csv.QUOTE_NONE)
    for entry in photos:
        output_writer.writerow(entry.return_csv_line())
