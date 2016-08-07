#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato
#

import logging

logger = logging.getLogger('qespresso')


def set_logger(loglevel=1, logfile=None):
    """
    Setup a basic logger with an handler and a formatter, using a
    corresponding numerical range [0..4], where a higher value means
    a more verbose logging. The loglevel value is mapped to correspondent
    logging module's value:

    LOG_CRIT=0 (syslog.h value is 2) ==> logging.CRITICAL
    LOG_ERR=1 (syslog.h value is 3) ==> logging.ERROR
    LOG_WARNING=2 (syslog.h value is 4) ==> logging.WARNING
    LOG_INFO=3 (syslog.h value is 6) ==> logging.INFO
    LOG_DEBUG=4 (syslog.h value is 7) ==> logging.DEBUG

    If a logfile name is passed then writes logs to file, instead of
    send logs to the standard output.

    :param loglevel: Simplified POSIX's syslog like logging level index
    :param logfile: Logfile name for non-scripts runs
    """
    global logger

    # Higher or lesser argument values are also mapped to DEBUG or CRITICAL
    effective_level = max(logging.DEBUG, logging.CRITICAL - loglevel * 10)

    logger.setLevel(effective_level)

    # Add the first new handler
    if not logger.handlers:
        if logfile is None:
            lh = logging.StreamHandler()
        else:
            lh = logging.FileHandler(logfile)
        lh.setLevel(effective_level)

        if effective_level <= logging.DEBUG:
            formatter = logging.Formatter("[%(levelname)s:%(module)s:%(funcName)s: %(lineno)s] %(message)s")
        elif effective_level <= logging.INFO:
            formatter = logging.Formatter("[%(levelname)s:%(module)s] %(message)s")
        else:
            formatter = logging.Formatter("%(levelname)s: %(message)s")

        lh.setFormatter(formatter)
        logger.addHandler(lh)
    else:
        for handler in logger.handlers:
            handler.setLevel(effective_level)
