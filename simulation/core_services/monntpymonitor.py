"""
Simple example custom service, used to drive shell commands on a node.
"""
from typing import Tuple

from core.nodes.base import CoreNode
from core.services.coreservices import CoreService, ServiceMode


class MonntpyMonitorService(CoreService):
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

    name: str = "MonntpyMonitorService"
    group: str = "moNNT.py"
    executables: Tuple[str, ...] = ()
    dependencies: Tuple[str, ...] = ()
    dirs: Tuple[str, ...] = ()
    configs: Tuple[str, ...] = ("monntpymonitor.sh", "countarticles.sh", )
    startup: Tuple[str, ...] = (
        "bash monntpymonitor.sh", )
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
        starting on the config filename that this informat ion will be used for. This
        must be defined, if "configs" are defined.

        :param node: core node that the service is being ran on
        :param filename: configuration file to generate
        :return: configuration file content
        """
        if filename == cls.configs[0]:
            cfg = f"""#!/bin/bash
export DISPLAY=:1.0 
sleep 1 
xterm -rv -title 'moNNT.py Articles' -e watch "bash countarticles.sh" &
"""
        elif filename == cls.configs[1]:
            cfg = f"""#!/bin/bash
echo
echo "NNTP Server Stats"
echo
echo "--------------------------------------"
echo "Node      | Articles in | Articles out"
echo "--------------------------------------"
total_in=0
total_out=0
for d in $SESSION_DIR/*.conf; do
    node=$(basename $d .conf)
    if [[ -f "$SESSION_DIR/$node.conf/monntpy.log" ]]; then
        art_in=$(rg --no-filename "Creating article entry" "$SESSION_DIR/$node.conf/monntpy.log" | wc -l)
        art_out=$(rg --no-filename "Sending article to DTNd and local DTN" "$SESSION_DIR/$node.conf/monntpy.log" | wc -l)
        total_in=$(echo "$total_in + $art_in" | bc)
        total_out=$(echo "$total_out + $art_out" | bc)
        printf "%-9s | %11s | %12s\n" "$node" "$art_in" "$art_out"
    fi
done
echo "--------------------------------------"
printf "%-9s | %11s | %12s\n" "Total" "$total_in" "$total_out"
"""
        return cfg

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
