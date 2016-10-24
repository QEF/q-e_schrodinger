# -*- coding: utf-8 -*-
#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato
#
"""
This module contain exception classes for Quantum Espresso package.
"""

import logging

logger = logging.getLogger('qespresso')


class QEspressoError(Exception):
    pass


class ConfigError(QEspressoError):
    """
    This exception is raised when there are errors with the validation
    of a XML configuration.
    """
    def __init__(self, message):
        Exception.__init__(self, message)
        logger.debug('!ConfigError: {0}'.format(message))


