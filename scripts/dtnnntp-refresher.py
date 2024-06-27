#!/usr/bin/python3

import datetime
import sqlite3
import os
from pathlib import Path
import sys

"""

Refresher script for articles in dtn7-showroom dtn-nntp scenario

Set the DB_PATH environment variable to the correct db location and
run this script. This will refresh the created_at datetimes of all articles
in the db. Articles will have different datetimes based on their IDs, with
lower IDs getting earlier datetimes, whereby
art2.id-art2.id == art2.datetime-art2.datetime.

"""


db_path = os.environ.get("DB_PATH")
if db_path is None:
    print("ERROR: Please set DB_PATH to correct db path and start script again")
    sys.exit(1)

print("Refreshing article created_at fields for dtn-nntp scenario")
print(f"  -> Opening database path {db_path}")
con = sqlite3.connect(
    db_path,
    detect_types=sqlite3.PARSE_DECLTYPES|sqlite3.PARSE_COLNAMES
)

with con:
    dts = con.execute("SELECT created_at, id FROM article").fetchall()
    num_art = len(dts)
    print(f"  -> Found {num_art} article entries")
    now = datetime.datetime.utcnow()

    for d in range(num_art):
        dts[d] = (now - datetime.timedelta(seconds=(num_art - d)), dts[d][1], )

    print("  -> Updating created_at datetimes")
    con.executemany("UPDATE article SET created_at=? WHERE id=?", dts)

    dts = con.execute("SELECT created_at FROM article").fetchall()
    print("  -> Done! Closing db connection")

con.close()