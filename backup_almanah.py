#!/bin/python

''' This piece of code just copy the Almanah database (plain or encrypted) into
a file in your home dir adding the current date (DDMMYYYY), useful for backup '''

import datetime
import os
import shutil
import sys

# Searching for Almanah databsae
diary_db = os.path.join(os.environ['HOME'], '.local', 'share', 'diary.db')
if not os.path.isfile(diary_db):
    diary_db = diary_db + '.encrypted'
    if not os.path.isfile(diary_db):
        sys.exit("Almanah database not found")
print "Almanah database founded on " + diary_db

backup_diary_db = os.path.join(os.environ['HOME'], os.path.basename(diary_db) + "." + datetime.date.today().strftime("%d%m%Y"))
print "Almanah database backup: " + backup_diary_db

shutil.copy(diary_db, backup_diary_db)

sys.exit()
