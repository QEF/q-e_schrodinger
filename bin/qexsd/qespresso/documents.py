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
import os.path

from .converters import PwInputConverter, PhononInputConverter, NebInputConverter
from .exceptions import ConfigError
from .xsdtypes import etree_node_to_dict, XmlDocument
from .xsdtypes.etree import etree_iter_path
logger = logging.getLogger('qespresso')


class QeDocument(XmlDocument):
    """
    Abstract class for XML schema based configurations.
    """
    def __init__(self, xsd_file, input_builder):
        super(QeDocument, self).__init__(xsd_file)
        self.input_builder = input_builder

        self.default_namespace = self.schema.target_namespace
        qe_nslist = list(map(self.namespaces.get, ['qes','neb','qes_ph']))
        if not self.default_namespace in qe_nslist:
            raise NotImplementedError("Converter not implemented for this schema {}".format(self.default_namespace) )

    def read_qe_input(self, filename):
        """
        Map from a Fortran input to XML old parameters to correspondent parameter in XML schema.

        :param filename:
        :return:
        """
        return self

    def write_qe_input(self, filename):
        """
        Write the XML configuration to a Fortran input.

        :param filename:
        :return:
        """
        with open(filename, mode='w+') as f:
            f.write(self.get_qe_input())

    def get_input_path(self):
        raise NotImplemented("This is an abstract implementation, use a subclass!")

    def get_qe_input(self, use_defaults=True):
        if self._document is None:
            raise ConfigError("Configuration not loaded!")

        qe_input = self.input_builder(xml_file=self._config_file)
        schema = self.schema
        input_path = self.get_input_path()
        input_root = self.find(input_path)

        # Extract values from input's subtree of the XML document
        for elem, path in etree_iter_path(input_root, path=input_path):
            rel_path = path.replace(input_path, '.')
            node_dict = etree_node_to_dict(elem, schema, root_path=path, use_defaults=use_defaults)
            logger.debug("Add input for node '{0}' with dict '{1}'".format(elem.tag, node_dict))

            # Convert attributes
            for attr_name, value in elem.attrib.items():
                logger.debug("Convert attribute '%s' of element '%s'" % (attr_name, path))
                path_key = '%s/%s' % (rel_path, attr_name)
                if path_key not in qe_input:
                    logger.debug("Attribute's path '%s' not in converter!" % path_key)
                    continue
                qe_input.set_path(path_key, elem.tag, node_dict)

            logger.debug("Convert element '%s'" % path)
            path_key = '%s/_text' % rel_path if schema.get_attributes(path) else rel_path
            if path_key not in qe_input:
                logger.debug("Element's path '%s' not in converter!" % path_key)
                continue
            qe_input.set_path(path_key, elem.tag, node_dict)

        if use_defaults:
            # Add defaults for elements not included in input XML subtree
            for path in filter(
                    lambda x: x.startswith(input_path) and self.find(x) is None,
                    schema.elements
            ):
                rel_path = path.replace(input_path, '.')
                tag = rel_path.rsplit('/', 1)[-1]
                xsd_attributes = schema.get_attributes(path)
                defaults_dict = {}
                defaults_path_keys = []

                try:
                    # Add default values for attributes
                    for attr_name, xsd_attribute in xsd_attributes.items():
                        default_value = xsd_attribute.get_default()
                        if default_value is not None:
                            path_key = '%s/%s' % (rel_path, attr_name)
                            xsd_type = xsd_attribute.xsd_type
                            value = xsd_type.decode(default_value)
                            defaults_dict[attr_name] = value
                            defaults_path_keys.append(path_key)
                except AttributeError:
                    pass

                default_value = schema.get_element_default(path)
                if default_value is not None:
                    path_key = '%s/_text' % rel_path if xsd_attributes else rel_path
                    xsd_type = schema.get_element_type(path)
                    value = xsd_type.decode(default_value)
                    defaults_dict[path_key.rsplit("/")[-1]] = value
                    defaults_path_keys.append(path_key)

                for path_key in defaults_path_keys:
                    qe_input.set_path(path_key, tag, defaults_dict)

        return qe_input.get_qe_input()

    def load_fortran_input(self, filename):
        if self._document is not None:
            raise ConfigError("Configuration not loaded!")

        # fortran_input = self.input_builder()
        return None


class PwDocument(QeDocument):
    """
    Class to manage PW XML documents.
    """
    def __init__(self):
        self._input_tag = 'input'
        super(PwDocument, self).__init__(
            xsd_file='%s/scheme/qes.xsd' % os.path.dirname(os.path.abspath(__file__)),
            input_builder=PwInputConverter
        )

    def get_input_path(self):
        return './input'


class PhononDocument(QeDocument):
    """
    Class to manage Phonon XML documents.
    """
    def __init__(self):
        self._input_tag = 'input'
        super(PhononDocument, self).__init__(
            xsd_file='%s/scheme/ph_temp.xsd' % os.path.dirname(os.path.abspath(__file__)),
            input_builder=PhononInputConverter
        )

    def get_input_path(self):
        return './inputPH'

    def get_qe_input(self, use_defaults=False):
        """
        overrides get_qe_input calling super get_qe_input with use_defaults set to False. 
        :param use_defaults: 
        :return: the input as obtained from its input builder
        """
        return super(PhononDocument, self).get_qe_input(use_defaults=use_defaults)


class NebDocument(QeDocument):
    """
    Class to manage NEB XML documents.
    """
    def __init__(self):
        self._input_tag = 'input'
        super(NebDocument, self).__init__(
            xsd_file='%s/scheme/qes_neb_temp.xsd' % os.path.dirname(os.path.abspath(__file__)),
            input_builder=NebInputConverter
        )

    def get_input_path(self):
        return './input'
