"""
Simple example custom service, used to drive shell commands on a node.
"""
from typing import Tuple

from core.nodes.base import CoreNode
from core.services.coreservices import CoreService, ServiceMode


class Dtn7zeroService(CoreService):
    """
    Example Custom CORE Service

    :cvar name: name used as a unique ID for this service and is required, no spaces
    :cvar group: allows you to group services within the GUI under a common name
    :cvar executables: executables this service depends on to function, if executable is
        not on the path, service will not be loaded
    :cvar dependencies: services that this service depends on for startup, tuple of
        service names
    :cvar dirs: directories that this service will create within a node
    :cvar configs: files that this service will generate, without a full path this file
        goes in the node's directory e.g. /tmp/pycore.12345/n1.conf/myfile
    :cvar startup: commands used to start this service, any non-zero exit code will
        cause a failure
    :cvar validate: commands used to validate that a service was started, any non-zero
        exit code will cause a failure
    :cvar validation_mode: validation mode, used to determine startup success.
        NON_BLOCKING    - runs startup commands, and validates success with validation commands
        BLOCKING        - runs startup commands, and validates success with the startup commands themselves
        TIMER           - runs startup commands, and validates success by waiting for "validation_timer" alone
    :cvar validation_timer: time in seconds for a service to wait for validation, before
        determining success in TIMER/NON_BLOCKING modes.
    :cvar validation_period: period in seconds to wait before retrying validation,
        only used in NON_BLOCKING mode
    :cvar shutdown: shutdown commands to stop this service
    """

    name: str = "dtn7zero"
    group: str = "Dtn7"
    executables: Tuple[str, ...] = ()
    dependencies: Tuple[str, ...] = ()
    dirs: Tuple[str, ...] = ()
    configs: Tuple[str, ...] = ('start-dtn7zero.py', )
    startup: Tuple[str, ...] = ("bash -c 'PYTHONUNBUFFERED=TRUE python3 ./start-dtn7zero.py &> dtn7zero.log'", )
    validate: Tuple[str, ...] = ()
    validation_mode: ServiceMode = ServiceMode.NON_BLOCKING
    validation_timer: int = 5
    validation_period: float = 0.5
    shutdown: Tuple[str, ...] = ()

    @classmethod
    def on_load(cls) -> None:
        """
        Provides a way to run some arbitrary logic when the service is loaded, possibly
        to help facilitate dynamic settings for the environment.

        :return: nothing
        """
        pass

    @classmethod
    def get_configs(cls, node: CoreNode) -> Tuple[str, ...]:
        """
        Provides a way to dynamically generate the config files from the node a service
        will run. Defaults to the class definition and can be left out entirely if not
        needed.

        :param node: core node that the service is being ran on
        :return: tuple of config files to create
        """
        return cls.configs

    @classmethod
    def generate_config(cls, node: CoreNode, filename: str) -> str:
        """
        Returns a string representation for a file, given the node the service is
        starting on the config filename that this information will be used for. This
        must be defined, if "configs" are defined.

        :param node: core node that the service is being ran on
        :param filename: configuration file to generate
        :return: configuration file content
        """
        
        return """#!/usr/bin/python3

# This script is a blueprint on how to configure and start dtn7zero

import socket
import sys
import time
import random

from dtn7zero import setup, register, register_group, start_background_update_thread, run_forever
from py_dtn7.bundle import PrimaryBlock


node_id = f"dtn://{socket.gethostname()}/"
node_endpoint = setup(node_id)


### Option: add receiver-endpoints
#def callback_receive(payload: bytes, full_source_uri: str, full_destination_uri: str, primary_block: PrimaryBlock):
#    print(f"received {payload} from {full_source_uri} addressed to {full_destination_uri}")
#
#receiver_endpoint = register("incoming", callback_receive)

### Option: add group-receiver-endpoints
#def callback_group(payload: bytes, full_source_uri: str, full_destination_uri: str, primary_block: PrimaryBlock):
#    print(f"received group message {payload.decode()} from {full_source_uri} addressed to {full_destination_uri}")
#
#group_endpoint = register_group("dtn://global-iot/~sensor-values", callback_group)


### Option: send messages
### Note: dtn7zero can update the framework with a background-thread, which leaves the main-thread for user defined code
###       -> recommended, as user defined code will not interfere with the framework update times (background-thread keeps running, as long as main-thread is running)
#start_background_update_thread()
#
#try:
#    while True:
#        time.sleep(1)
#        node_endpoint.send(f"{random.randint(-10, 55)}".encode(), "dtn://global-iot/~sensor-values")
#except KeyboardInterrupt:
#    pass

### Default-Option: relay-node
### Note: dtn7zero can also be started synchronously, with a separate main method for user defined code
###       -> use carefully, as main() is expected to not block for a significant amount of time
def main():
    pass

run_forever(main, loop_callback_interval_milliseconds=1000)

"""

    @classmethod
    def get_startup(cls, node: CoreNode) -> Tuple[str, ...]:
        """
        Provides a way to dynamically generate the startup commands from the node a
        service will run. Defaults to the class definition and can be left out entirely
        if not needed.

        :param node: core node that the service is being ran on
        :return: tuple of startup commands to run
        """
        return cls.startup

    @classmethod
    def get_validate(cls, node: CoreNode) -> Tuple[str, ...]:
        """
        Provides a way to dynamically generate the validate commands from the node a
        service will run. Defaults to the class definition and can be left out entirely
        if not needed.

        :param node: core node that the service is being ran on
        :return: tuple of commands to validate service startup with
        """
        return cls.validate
