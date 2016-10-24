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

from ..utils.logger import set_logger

logger = logging.getLogger('qespresso')


def etree_iter_path(node, tag=None, path='.'):
    """
    A version of ElementTree node's iter function that return a couple
    with node and his relative path.

    :param node:
    :param tag:
    :param path:
    :return:
    """
    if tag == "*":
        tag = None
    if tag is None or node.tag == tag:
        yield node, path
    for child in node:
        _child_path = '%s/%s' % (path, child.tag)
        for child, child_path in etree_iter_path(child, tag, path=_child_path):
            yield child, child_path


def etree_to_dict(etree, xml_schema, dict_class=dict, spaces_for_tab=4, use_defaults=True):
    set_logger(1)
    root_node = etree.getroot()
    ret_dict = etree_node_to_dict(
        root_node, xml_schema, dict_class=dict_class, spaces_for_tab=spaces_for_tab, use_defaults=use_defaults
    )
    set_logger(1)
    return ret_dict


def etree_node_to_dict(root_node, xml_schema, root_path='.', dict_class=dict, spaces_for_tab=4, use_defaults=True):

    def _etree_node_to_dict(node, node_path):
        node_dict = dict_class()
        logger.debug("Decode node '%s' with path '%s'" % (node.tag, node_path))

        if node.attrib:
            # if we have attributes, decode them
            logger.debug("Decode attributes of element '{0}': {1}".format(node.tag, node.items()))
            attr_dict = dict([
                (attr, xml_schema.get_attribute_type(attr, node_path).decode(value))
                for attr, value in node.items()
            ])
            if use_defaults:
                # Add default values for missing attributes
                for attr in list(set(xml_schema.get_attributes(node_path)) - set(node.keys())):
                    default_value = xml_schema.get_attribute_default(attr, node_path)
                    if default_value is not None:
                        xsd_type = xml_schema.get_attribute_type(attr, node_path)
                        attr_dict[attr] = xsd_type.decode(default_value)
            node_dict.update(attr_dict)

        for child in node:
            # recursively add the element's children
            new_item = _etree_node_to_dict(child, node_path='%s/%s' % (node_path, child.tag))
            if child.tag in node_dict:
                # found duplicate tag, force a list
                if isinstance(node_dict[child.tag], list):
                    # append to existing list
                    node_dict[child.tag].append(new_item)
                else:
                    # convert to list
                    node_dict[child.tag] = [node_dict[child.tag], new_item]
            else:
                # only one, directly set the dictionary
                node_dict[child.tag] = new_item

        if node.text is None:
            if use_defaults:
                text = xml_schema.get_element_default(node_path) or ''
            else:
                text = ''
        elif spaces_for_tab is None:
            text = node.text.strip()
        else:
            text = node.text.strip().replace('\t', ' ' * spaces_for_tab)

        # Get the XSD type for node's text decoding
        xsd_type = xml_schema.get_element_type(node_path)
        logger.debug("Decode '{0}' to type '{1}'".format(
            ' '.join(text.replace('\n', r'\n').replace('\t', ' ').split()), xsd_type.name)
        )

        if node_dict:
            # if we have a dictionary add the text as a dictionary value (if there is any)
            if len(text) > 0:
                node_dict['_text'] = xsd_type.decode(text)
                logger.debug("Text decoded to: {0}".format(node_dict['_text']))
        else:
            # if we don't have child nodes or attributes, just set the text
            node_dict = xsd_type.decode(text)
            logger.debug("Text decoded to: {0}".format(node_dict))

        return node_dict

    ret_dict = dict_class({root_node.tag: _etree_node_to_dict(root_node, root_path)})
    return ret_dict


#
# Two functions to convert an element into a dictionary and back, an adaptation of code taken
# from  http://code.activestate.com/recipes/573463-converting-xml-to-dictionary-and-back/.
#
def xml_to_dict(root_node, dict_class=dict, spaces_for_tab=4):
    """
    Converts an XML ElementTree to a dictionary

    :param root_node: Root node (Element) of the XML ElementTree
    :param dict_class: Dictionary type subclass to create
    :param spaces_for_tab: If not None, substitute tab characters with N spaces
    :return: Dictionary representing the XML document
    """
    def _element_to_dict(node):
        node_dict = dict_class()

        if len(node.items()) > 0:
            # if we have attributes, set them
            node_dict.update(dict(node.items()))

        for child in node:
            # recursively add the element's children
            new_item = _element_to_dict(child)
            if child.tag in node_dict:
                # found duplicate tag, force a list
                if isinstance(node_dict[child.tag], list):
                    # append to existing list
                    node_dict[child.tag].append(new_item)
                else:
                    # convert to list
                    node_dict[child.tag] = [node_dict[child.tag], new_item]
            else:
                # only one, directly set the dictionary
                node_dict[child.tag] = new_item

        if node.text is None:
            text = ''
        elif spaces_for_tab is None:
            text = node.text.strip()
        else:
            text = node.text.strip().replace('\t', ' ' * spaces_for_tab)

        if node_dict:
            # if we have a dictionary add the text as a dictionary value (if there is any)
            if len(text) > 0:
                node_dict['_text'] = text
        else:
            # if we don't have child nodes or attributes, just set the text
            node_dict = text

        return node_dict

    # if not isinstance(root_node, etree.Element):
    #    raise TypeError('Expected ElementTree.Element')

    return dict_class({root_node.tag: _element_to_dict(root_node)})


def dict_to_xml(xml_dict):
    """
    Converts a dictionary into an XML ElementTree Element
    """
    def _dict_to_element(parent, dictitem):
        assert not isinstance(dictitem, list)

        if isinstance(dictitem, dict):
            for (tag, child) in dictitem.items():
                if str(tag) == '_text':
                    parent.text = str(child)
                elif isinstance(child, list):
                    # iterate through the array and convert
                    for listchild in child:
                        elem = ElementTree.Element(tag)
                        elem.tail = '\n'
                        parent.append(elem)
                        _dict_to_element(elem, listchild)
                else:
                    elem = ElementTree.Element(tag)
                    elem.tail = '\n'
                    parent.append(elem)
                    _dict_to_element(elem, child)
        else:
            parent.text = str(dictitem)

    root_tag = xml_dict.keys()[0]
    root_element = ElementTree.Element(root_tag)
    _dict_to_element(root_element, xml_dict[root_tag])
    return root_element
