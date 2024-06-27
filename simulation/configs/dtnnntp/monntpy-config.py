from pathlib import Path
import os
import subprocess

"""
For running this in CORE Emu Lab, we need to change some of moNNT.py's configuration
This enables us to load a standard config.toml into the applications working path
and then substitute some elements with dynamically generated values that are taken
from the environment or passed to start-monttpy.sh

"""

SENDER_EMAIL = os.environ.get('SENDER_EMAIL')
SESSION_DIR = os.environ.get('SESSION_DIR')
HOSTNAME = subprocess.run(["hostname"], stdout=subprocess.PIPE).stdout.decode().strip()


config = {
        "backend": {"db_url": f"sqlite://{SESSION_DIR}/{HOSTNAME}.conf/db.sqlite3"},
        "dtnd": {
            "host": "http://127.0.0.1",
            "node_id": f"dtn://{HOSTNAME}/",
            "port": 3000,
            "rest_path": "",
            "ws_path": "/ws",
            "multi_user": False,
        },
        "backoff": {
            "initial_wait": 0.1,
            "max_retries": 20,
            "reconnection_pause": 300,
            "constant_wait": 0.75,
        },
        "bundles": {"lifetime": 86400000, "delivery_notification": False, "compress_body": False},
        "usenet": {
            "expiry_time": 2419200000,
            "email": SENDER_EMAIL,
            "newsgroups": [
                "monntpy.dev",
                "monntpy.offtopic",
                "monntpy.users.tu-darmstadt",
                "monntpy.users.uni-frankfurt",
                "monntpy.users.jlu-giessen",
            ],
        },
    }