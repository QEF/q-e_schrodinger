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
This module contains XMLSchema class for xsdtypes package
"""

import logging
from xml.etree import ElementTree

from .exceptions import FileFormatError, XMLSchemaValidationError
from .xsdtypes import *
from .xsdtypes import _ATTRIBUTE_TAG


logger = logging.getLogger('qespresso')


class XMLSchema(object):
    """
    Class to wrap an XML Schema for types lookups.
    """
    XML_SCHEMA_NAMESPACES = {
        'xsd': XSD_NAMESPACE_PATH,
        'xs': XSD_NAMESPACE_PATH,
        'xsi': XSI_NAMESPACE_PATH
    }

    def __init__(self, xsd_file):
        self._xsd = None
        self._xml_schema = None

        self.namespaces = self.XML_SCHEMA_NAMESPACES.copy()
        "Namespaces used by XSD file"

        self.target_namespace = None
        "Target namespace URI"

        self.target_prefix = ''
        "Namespace prefix for declarations"

        # Temporary mods for preserve types declaration order for Fortran generation
        from collections import OrderedDict
        self.types = OrderedDict()  # {}
        "Map XSD global types to XSDType instance"

        self.attributes = {}
        "Map XSD global attributes to XSDType instance"

        self.attribute_groups = {}
        "Group XSD attributes definitions"

        self.elements = {}
        "Map XSD global elements to XSDType instance"

        self.groups = {}
        "Group XSD elements definitions"

        try:
            self._xsd = ElementTree.parse(xsd_file)
        except ElementTree.ParseError as e:
            raise FileFormatError('XML', xsd_file, e.msg)
        else:
            self._xsd_file = xsd_file

        try:
            self._xml_schema = ElementTree.XMLSchema(self._xsd)
        except AttributeError:
            logger.info("XSD validation is not available!")
        except ElementTree.XMLSchemaError as e:
            logger.error("XSD schema error: %s " % repr(e))
            raise

        # Set the namespaces information
        schema_elem = self.getroot()
        self.target_namespace = schema_elem.attrib['targetNamespace']
        logger.debug("Target namespace: %s" % self.target_namespace)

        if hasattr(schema_elem, 'nsmap'):
            # lxml loaded
            for prefix, namespace in schema_elem.nsmap.items():
                self.namespaces[prefix or ''] = namespace  # Change None key with ''
        else:
            # xml.etree.ElementTree loaded: need to get namespaces from file
            for event, node in ElementTree.iterparse(self._xsd_file, events=['start-ns']):
                self.namespaces[node[0]] = node[1]
        logger.debug('Namespaces: {0}'.format(self.namespaces))

        # Set namespace prefix for schema declarations
        for prefix in self.namespaces:
            if self.namespaces[prefix] == self.target_namespace:
                self.target_prefix = prefix
                if prefix:
                    prefix = '%s:' % prefix
                break
        else:
            prefix = ''
        logger.debug("Use prefix '%s' for schema declarations" % prefix)

        # Build lookup maps using XSD declarations
        logger.debug("### Add global simple types ###")
        add_xsd_types_to_dict(self.findall('./xsd:simpleType'), self.types,
                              xsd_simple_type_factory, xsd_types=self.types, prefix=prefix)

        logger.debug("### Add global attributes ###")
        counter = 0
        for elem in self.findall('./xsd:attributes'):
            self.attributes.update(
                [xsd_attribute_type_factory(elem, xsd_types=self.types, prefix=prefix)]
            )
            counter += 1
        logger.debug("%d global attributes added" % counter)

        logger.debug("### Add attribute groups ###")
        counter = 0
        for elem in self.findall('./xsd:attributeGroup'):
            try:
                name = elem.attrib['name']
            except KeyError:
                raise XMLSchemaValidationError("Missing 'name' attribute for {}".format(elem))
            if name in self.attribute_groups:
                raise XMLSchemaValidationError("Duplicate attributeGroup: {}".format(name))

            self.attribute_groups[name] = OrderedDict([
                xsd_attribute_type_factory(child, xsd_types=self.types, prefix=prefix)
                for child in elem if child.tag == _ATTRIBUTE_TAG
            ])
            counter += 1
        logger.debug("%d attribute groups added" % counter)

        logger.debug("### Add global complex types ###")
        add_xsd_types_to_dict(self.findall('./xsd:complexType'), self.types,
                              xsd_complex_type_factory, xsd_types=self.types,
                              prefix=prefix, xsd_attributes=self.attributes)

        logger.debug("### Add content model groups ###")
        counter = 0
        for elem in self.findall('./xsd:group'):
            try:
                name = elem.attrib['name']
            except KeyError:
                raise XMLSchemaValidationError("Missing 'name' attribute for {}".format(elem))
            if name in self.groups:
                raise XMLSchemaValidationError("Duplicate group: {}".format(name))

            from .xsdtypes import xsd_group_factory
            self.groups[name] = xsd_group_factory(
                elem, xsd_groups=self.groups, xsd_types=self.types,
                xsd_attributes=self.attributes, xsd_attribute_groups=self.attribute_groups
            )
            counter += 1
        logger.debug("%d XSD named groups added" % counter)

        logger.debug("### Add elements starting from root element(s) ###")
        for elem in self.findall('./xsd:element'):
            xsd_element_factory(elem, parent_path=None, prefix=prefix, xsd_elements=self.elements,
                                xsd_types=self.types, xsd_attributes=self.attributes,
                                xsd_attribute_groups=self.attribute_groups)
        logger.debug("%d XSD elements added" % len(self.elements))

    def get_type(self, type_name):
        """
        Return the XSD type instance corresponding to the argument.

        :param type_name: Name of the type. Types in the schema's
        namespace have to be provided with namespace URI or prefix.
        Unqualified names are interpreted as typed of xsd/xs namespace.
        :return: XSDType or XSDSimpleType instance
        """
        logger.debug("XSD type name: '{0}'".format(type_name))
        return xsd_get_type(type_name, self.types)

    def get_element(self, element_path):
        element_path = element_path.replace('/{%s}' % self.target_namespace, '/')\
            .replace('/%s:' % self.target_prefix, '/')
        try:
            return self.elements[element_path]
        except KeyError:
            raise ValueError("Element '%s' not found in XSD schema!" % element_path)

    def get_element_tag(self, element_path):
        """
        Return the XSD type instance of the element.

        :param element_path: The absolute path to the element. Schema's
        namespace prefixes or URIs are stripped from the path.
        :return: XSDType, XSDSimpleType or XSDComplexType instance
        """
        try:
            name = self.get_element(element_path).name
            if self.target_prefix and name.startswith('%s:' % self.target_prefix):
                return name.replace('%s:' % self.target_prefix, '')
        except KeyError:
            raise ValueError("Element '%s' not found in XSD schema!" % element_path)

    def get_attributes(self, element_path):
        element = self.get_element(element_path)
        try:
            return element.xsd_type.attributes
        except AttributeError:
            return dict()

    def get_element_type(self, element_path):
        """
        Return the XSD type instance of the element.

        :param element_path: The absolute path to the element. Schema's
        namespace prefixes or URIs are stripped from the path.
        :return: XSDType, XSDSimpleType or XSDComplexType instance
        """
        try:
            return self.get_element(element_path).get_type()
        except KeyError:
            raise ValueError("Element '%s' not found in XSD schema!" % element_path)

    def get_attribute_type(self, attribute_name, element_path):
        """
        Return the XSD type instance of the attribute.

        :param attribute_name: The name of the attribute. Schema's
        namespace prefix or URI is stripped from the name.
        :param element_path: The absolute path to the element. Schema's
        namespace prefixes or URIs are stripped from the path.
        :return: XSDType or XSDSimpleType instance
        """
        attribute_name = attribute_name.replace('{%s}' % self.target_namespace, '/')\
            .replace('%s:' % self.target_prefix, '/')

        try:
            xsd_type = self.get_element(element_path).xsd_type.attributes[attribute_name].xsd_type
        except KeyError:
            try:
                return XSI_ATTRIBUTES[attribute_name].xsd_type
            except KeyError:
                raise ValueError("Attribute '%s' not found in XSD schema!" % attribute_name)
        else:
            logger.debug("Map attribute '%s' to type '%s'" % (attribute_name, xsd_type.name))
            return xsd_type

    def get_element_default(self, element_path):
        """
        Return the default of the element.

        :param element_path: The absolute path to the element. Schema's
        namespace prefixes or URIs are stripped from the path.
        :return: XSDType, XSDSimpleType or XSDComplexType instance
        """
        return self.get_element(element_path).get_default()

    def get_attribute_default(self, attribute_name, element_path):
        """
        Return the XSD type instance of the attribute.

        :param attribute_name: The name of the attribute. Schema's
        namespace prefix or URI is stripped from the name.
        :param element_path: The absolute path to the element. Schema's
        namespace prefixes or URIs are stripped from the path.
        :return: XSDType or XSDSimpleType instance
        """
        try:
            xsd_attribute = self.get_attributes(element_path)[attribute_name]
        except KeyError:
            msg = "Attribute '%s' not found in XSD element '%s'!"
            raise ValueError(msg % (attribute_name, element_path))
        else:
            return xsd_attribute.get_default()

    def find(self, path, namespaces=None):
        """
        Find first matching element by tag name or path.

        :param path: is a string having either an element tag or an XPath,
        :param namespaces: is an optional mapping from namespace prefix to full name.
        :return: the first matching element, or None if no element was found
        """
        namespaces = namespaces or {}
        namespaces.update(self.namespaces)
        return self._xsd.find(path, namespaces)

    def findall(self, path, namespaces=None):
        """
        Find all matching subelements by tag name or path.

        :param path: is a string having either an element tag or an XPath,
        :param namespaces: is an optional mapping from namespace prefix to full name.
        :return: the first matching element, or None if no element was found
        """
        namespaces = namespaces or {}
        namespaces.update(self.namespaces)
        return self._xsd.findall(path, namespaces)

    def getroot(self):
        """Return root element of the XML schema tree."""
        return self._xsd.getroot()

    def iselement(self, elem):
        """
        Checks if an element instance appears to be a valid
        element object.
        :param elem:
        """
        return self._xsd.iselement(elem)

    def iter(self, tag=None):
        """
        Create and return an iterator that loops over all elements
        in this tree, in document order.

        :param tag: is a string with the tag name to iterate over
        (default is to return all elements).
        """
        if tag is not None and tag[0] != '{':
            tag = '{%s}%s' % (XSD_NAMESPACE_PATH, tag)
        return self._xsd.iter(tag)

    def validate(self, xml_config):
        """
        Validate the configuration with XSD and with optional parameter dependencies.
        :param xml_config:
        """
        try:
            import lxml.etree as ET
        except ImportError:
            logger.debug("")
            return

        xsd = ET.parse(self._xsd_file)
        xml_schema = ET.XMLSchema(xsd)

        try:
            xml_schema.assertValid(xml_config.tostring())
        except AttributeError:
            logger.warning("XML Schema validation not available!")
        except ET.DocumentInvalid as e:
            raise XMLSchemaValidationError(e)
