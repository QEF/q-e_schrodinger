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
This module contains XMLDocument class for xsdtypes package
"""

import logging
from xml.etree import ElementTree

from .exceptions import FileFormatError, XMLSchemaValidationError
from .xmlschema import XMLSchema

logger = logging.getLogger('qespresso')


class XmlDocument(object):
    """
    Generic XML schema based document.

    A XSD schema is needed for checking types, validation of the configuration
    and for lookup of default values of the attributes. Full schema's validation
    is available only if lxml library is installed.

    Supported files data format for configuration are XML, YAML and JSON.
    """
    def __init__(self, xsd_file):
        self._document = None
        self._config_file = None
        self._file_format = None
        self._xsd_file = xsd_file

        try:
            self.schema = XMLSchema(xsd_file)
        except (IOError, FileFormatError) as err:
            logger.error('XML Schema not available: %s' % err)
            self.schema = None
            raise
        self.namespaces = self.schema.namespaces

    def read(self, filename, data_format='XML'):
        """
        Read configuration from a text file in a specific data format.

        :param filename: Name of the text file containing the configuration
        :param data_format: Input file data format (XML, JSON or YAML)
        """
        data_format = data_format.upper()
        old_config = (self._document, self._config_file)
        try:
            if data_format == 'XML':
                self._document = self.parse_xml(filename)
            elif data_format == 'YAML':
                self._document = self.parse_yaml(filename)
            elif data_format == 'JSON':
                self._document = self.parse_json(filename)
            else:
                raise ValueError("'input_format' argument must be 'XML', 'YAML' or 'JSON'")
        except (FileFormatError, XMLSchemaValidationError) as e:
            raise
        else:
            self._config_file = filename

        # Validation of the new ElementTree structure (valid
        try:
            self.validate()
        except XMLSchemaValidationError as e:
            self._document, self._config_file = old_config
            raise

    def parse_xml(self, filename):
        """
        Return an ElementTree object representing an XML file
        """
        try:
            return ElementTree.parse(filename)
        except ElementTree.ParseError as e:
            raise FileFormatError('XML', filename, e)

    def parse_json(self, filename):
        """
        Build an ElementTree object representing a YAML file
        """
        logger.warning("JSON read is a TODO!")

    def parse_yaml(self, filename):
        """
        Build an ElementTree object representing a YAML file
        """
        logger.warning("YAML read is a TODO!")

    def from_dict(self):
        """
        Build an ElementTree object from a dictionary
        """

    def validate(self, filename=None):
        if filename is not None:
            try:
                self.validate()
            except XMLSchemaValidationError as e:
                e.message = "Invalid XML file '%s': %s" % (filename, e.message)
                raise
            else:
                self._config_file = filename

        self.schema.validate(self._document)
        self.extra_validations(self._document)

    def extra_validations(self, xlm_tree):
        """
        Hook for ad-hoc validations of dependencies between parameters that
        are not explainable with the XSD schema.
        """

        pass

    def write(self, filename, output_format='XML'):
        """
        Write configuration to a text file in a specific data format.

        :param filename:
        :param output_format:
        :return:
        """
        if self._document is None:
            logger.error("No configuration loaded!")
            return

        output_format = output_format.upper()
        if output_format == 'XML':
            pass
        elif output_format == 'YAML':
            logger.warning("YAML write is a TODO!")
        elif output_format == 'JSON':
            logger.warning("JSON write is a TODO!")
        else:
            raise ValueError("Accepted output_format are: 'XML'(default), 'YAML' and 'JSON'!")

    def read_string(self, text):
        self._document = ElementTree.fromstring(text)

    def get(self, qualified_name):
        section, _, item = qualified_name.partition(".")
        query = "./{0}/{1}".format(section, item)
        print(query)
        node = self._document.find(query)
        if node is None:
            return
        return node.text

    def __getitem__(self, section):
        query = "./{0}".format(section)
        parent = self._document.find(query)
        return dict((item.tag, item.text) for item in parent)

    def to_dict(self):
        from .etree import etree_to_dict
        return etree_to_dict(self._document, self.schema)

    def to_json(self):
        """Converts the configuration to to json."""
        import json
        return json.dumps(self.to_dict(), sort_keys=True, indent=4)

    def to_yaml(self):
        """Converts the configuration to to json."""
        import yaml
        return yaml.dump(self.to_dict(), default_flow_style=False)

    # ElementTree API wrappers

    def iter(self, tag=None):
        return self._document.iter(tag)

    def find(self, path, namespaces=None):
        """
        Find first matching element by tag name or path.

        :param path: is a string having either an element tag or an XPath,
        :param namespaces: is an optional mapping from namespace prefix to full name.
        :return: the first matching element, or None if no element was found
        """
        namespaces = namespaces or {}
        namespaces.update(self.namespaces)
        return self._document.find(path, namespaces)

    def findall(self, path, namespaces=None):
        """
        Find all matching subelements by tag name or path.

        :param path: is a string having either an element tag or an XPath,
        :param namespaces: is an optional mapping from namespace prefix to full name.
        :return: the first matching element, or None if no element was found
        """
        namespaces = namespaces or {}
        namespaces.update(self.namespaces)
        return self._document.findall(path, namespaces)
