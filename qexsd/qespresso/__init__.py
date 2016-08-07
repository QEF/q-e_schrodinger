# -*- coding: utf-8 -*-
#
# Copyright (c), 2015-2016, Quantum Espresso Foundation and SISSA (Scuola
# Internazionale Superiore di Studi Avanzati). All rights reserved.
# This file is distributed under the terms of the MIT License. See the
# file 'LICENSE' in the root directory of the present distribution, or
# http://opensource.org/licenses/MIT.
# Authors: Davide Brunato
#
import logging

# QEspresso imports
from .documents import QeDocument, PwDocument, PhononDocument, NebDocument
from .converters import RawInputConverter, PwInputConverter, PhononInputConverter, NebInputConverter
from .exceptions import ConfigError
from .xsdtypes import XSD_BUILTIN_TYPES, XMLSchema
from .utils.logger import set_logger

logger = logging.getLogger('qespresso')
set_logger(1)

__all__ = [
    'set_logger', 'ConfigError',
    'QeDocument', 'PWConfiguration', 'PhononDocument',
    'RawInputConverter', 'PwInputConverter', 'PhononInputConverter'
]
