"""
Simple example custom service, used to drive shell commands on a node.
"""
from typing import Tuple

from core.nodes.base import CoreNode
from core.services.coreservices import CoreService, ServiceMode


class DtndService(CoreService):
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

    name: str = "dtnd"
    group: str = "Dtn7"
    executables: Tuple[str, ...] = ()
    dependencies: Tuple[str, ...] = ('DefaultMulticastRoute', )
    dirs: Tuple[str, ...] = ()
    configs: Tuple[str, ...] = ('dtnd.toml', 'start-dtnd.sh')
    startup: Tuple[str, ...] = (
        "bash -c 'dtnd -c dtnd.toml &> dtnd.log'", )
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
        dtnd_config = f"""
# Example config file for dtn7 daemon
debug = false
nodeid = "{node.name}"
# Enables advertisement of the beaon period in discovery beacons
beacon-period = true

generate-status-reports = false

webport = 3000

workdir = "."

db = "mem"

[routing]
strategy = "epidemic"

[core]
# the janitor is responsible for cleaning the bundle buffer
# and schedule resubmissions.
# a value of 0 deactives the janitor
janitor = "10s"


[discovery]
# interval of 0 deactives discovery service
interval = "2s"
peer-timeout = "20s"

[convergencylayers]

cla.0.id = "mtcp"
cla.0.port = 16163

# Define user specified discovery targets to send announcement beacons to, if not specified the default targets "224.0.0.26:3003" for IPv4 and "[FF02::1]:3003" will be used
# If a IPv4 address is specified the IPv4 flag has to be enabled, same goes for specifying an IPv6 address
# [discovery_destinations]
#
# target.0.destination = "224.0.0.27:3004"
#
# target.1.destination = "[FF02::1]:3004"


# Define user specified services that will be advertised with discovery beacons
# Each service takes a u8 tag and a payload value who's content depends on the used tag
#[services]
# So far 4 Tags are defined and can be used like this:
#
# Tag 63 advertises a custom, unformatted string message
# service.0.tag = 63
# service.0.payload = "This is a custom string message"
#
# Tag 127 advertises 2 floating point numbers represented as geographical location in latitude/longitude
# service.1.tag = 127
# service.1.payload = "52.32 24.42"
#
# Tag 191 advertises 1 integer represented as battery level in %
# service.2.tag = 191
# service.1.payload = "75"
#
# Tag 255 advertises an address represented like this: (Street Number PostalNumber City CountryCode)
# service.3.tag = 255
# service.3.payload = "Samplestreet 42 12345 SampleCity SC"

[statics]
#peers = [
#    "mtcp://192.168.2.101/testnode",    
#]

[endpoints]
# local endpoints are always reachable under dtn://<nodeid>/<localname>
#local.0 = "incoming"
#group.0 = "dtn://hometown/weather" # atm ignored

        """
        cfg = "#!/bin/sh\n"
        if filename == cls.configs[0]:
            cfg = "# auto-generated by DtndService (dtnd.py)\n" + dtnd_config
        elif filename == cls.configs[1]:
            cfg = f"#!/bin/sh\ndtnd -c {cls.configs[0]} &> dtnd.log\n"
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
        #cmd = f"bash start-dnd.sh"
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
