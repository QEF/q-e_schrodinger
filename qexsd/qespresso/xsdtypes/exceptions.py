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
This module contains exception classes for xsdtypes package
"""

import logging
logger = logging.getLogger('qespresso')


class XMLDecodeError(ValueError):
    """Raised when a XML value string is not decodable to a Python object."""
    pass


class XMLEncodeError(ValueError):
    """Raised when an object is not encodable to an XML value string."""
    pass


class FileFormatError(ValueError):
    """
    This exception is raised when a XML file has syntax errors.
    """
    def __init__(self,  data_format, filename, message=None):
        if message is None:
            message = "Syntax error in %s file '%s'" % (data_format, filename)
        else:
            message = "Syntax error in %s file '%s': %s" % (data_format, filename, message)
        Exception.__init__(self, message)


class XMLValueError(ValueError):
    """
    Raised when the decoded value is non validated.
    """
    def __init__(self,  value, type_name):
        message = "Invalid value '{0}' for XSD type '{1}'".format(value, type_name)
        Exception.__init__(self, message)


class XMLSchemaValidationError(ValueError):
    """
    This exception is raised when the XML configuration is not validated
    with the XSD schema.
    """
    def __init__(self, message):
        Exception.__init__(self, message)
        logger.debug('!XMLSchemaValidationError: {0}'.format(message))
