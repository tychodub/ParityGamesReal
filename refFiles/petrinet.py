# !/usr/bin/python3
# -*- coding_ utf-8 -*-

from array import array
from collections import Counter
from typing import Dict, Text, List
from lxml import etree
import sys
import threading
from time import time_ns, sleep


class PetriNetModel(object):
    def __init__(self, places, transitions, p_ids, t_ids, tin, tout, initial):
        self.places = places
        self.transitions = transitions
        self.p_ids = p_ids
        self.t_ids = t_ids
        self.tin = [Counter(x) for x in tin]
        self.tout = [Counter(x) for x in tout]
        self.initial = tuple(initial)

    def initial_states(self):
        yield self.initial

    def successors(self, marking):
        for i in range(len(self.tin)):
            if self.is_enabled(i, marking):
                new_marking = array('I', marking)
                for x, y in self.tin[i].items():
                    new_marking[x] -= y
                for x, y in self.tout[i].items():
                    new_marking[x] += y
                yield self.transitions[i], tuple(new_marking)

    def is_enabled(self, transition, marking):
        for x, y in self.tin[transition].items():
            if marking[x] < y:
                return False
        return True


def parse_pnml_file(path):
    parser = etree.XMLParser(remove_comments=True, ns_clean=True)
    tree = etree.parse(path, parser=parser)

    # Strip annoying namespace tags
    for elem in tree.getiterator():
        elem.tag = etree.QName(elem).localname
    etree.cleanup_namespaces(tree)

    root = tree.getroot()
    nets = []  # list for parsed PetriNet objects

    for net_node in root.iter('net'):
        # parse transitions
        transitions = [t.get('id') for t in net_node.iter('transition')]
        t_ids = {x: i for i, x in enumerate(transitions)}

        # parse places
        places = []
        p_ids: Dict[Text, int] = {}
        initial = []
        for i, p in enumerate(net_node.iter('place')):
            places.append(p.get('id'))
            p_ids[p.get('id')] = i
            place_initial = 0
            for child in p:
                if "initialMarking" in child.tag:
                    for child2 in child:
                        if child2.tag == "text":
                            place_initial = int(child2.text)
            initial.append(place_initial)

        initial = array('I', initial)
        transitions_in: List[List[int]] = [[] for _ in t_ids]
        transitions_out: List[List[int]] = [[] for _ in t_ids]

        # parse arcs
        for arc_node in net_node.iter('arc'):
            source: Text = arc_node.get('source')
            target: Text = arc_node.get('target')
            if source in p_ids and target in t_ids:
                transitions_in[t_ids[target]].append(p_ids[source])
            elif source in t_ids and target in p_ids:
                transitions_out[t_ids[source]].append(p_ids[target])

        # create PetriNet object
        nets.append(PetriNetModel(places, transitions, p_ids, t_ids, transitions_in, transitions_out, initial))

    return nets


class PNExpression(object):
    def evaluate(self, _):
        pass


class IsFireable(PNExpression):
    def __init__(self, net, *args):
        self.net = net
        self.transitions = args

    def evaluate(self, marking):
        for t in self.transitions:
            if self.net.is_enabled(t, marking):
                return True
        return False

    def __repr__(self):
        return "is-fireable({})".format(self.transitions)


class TokensCount(PNExpression):
    def __init__(self, net, *args):
        self.net = net
        self.places = args

    def evaluate(self, marking):
        return sum([marking[p] for p in self.places])

    def __repr__(self):
        return "tokens-count({})".format(self.places)


class IntegerLE(PNExpression):
    def __init__(self, net, lhs, rhs):
        self.net = net
        self.lhs = lhs
        self.rhs = rhs

    def evaluate(self, marking):
        return self.lhs.evaluate(marking) <= self.rhs.evaluate(marking)

    def __repr__(self):
        return "{} <= {}".format(self.lhs, self.rhs)


class IntegerConstant(PNExpression):
    def __init__(self, net, value):
        self.net = net
        self.value = value

    def evaluate(self, marking):
        return self.value

    def __repr__(self):
        return str(self.value)


class IntegerSum(PNExpression):
    def __init__(self, net, *args):
        self.net = net
        self.subexpressions = args

    def evaluate(self, marking):
        return sum([x.evaluate(marking) for x in self.subexpressions])

    def __repr__(self):
        return " + ".join(self.subexpressions)


class IntegerDifference(PNExpression):
    def __init__(self, net, lhs, rhs):
        self.net = net
        self.lhs = lhs
        self.rhs = rhs

    def evaluate(self, marking):
        return self.lhs.evaluate(marking) - self.rhs.evaluate(marking)

    def __repr__(self):
        return "{} - {}".format(self.lhs, self.rhs)


class PropertyXMLParser(object):
    def __init__(self, net):
        self.props = []
        self.dag = LTLDAG()
        self.net = net

    def __call__(self, node):
        return self.props, LTLDAGNode(self.dag, self.parse(node))

    def parse(self, node):
        tag = node.tag
        if node.tag == 'formula' or node.tag == 'all-paths':
            return self.parse(node.getchildren()[0])
        elif node.tag == 'not':
            sub = self.parse(node.getchildren()[0])
            return self.dag.make_reduce('~', sub)
        elif node.tag == 'and':
            subs = [self.parse(x) for x in node.getchildren()]
            res = 1
            for i, s in enumerate(subs):
                if i == 0:
                    res = s
                else:
                    res = self.dag.make_reduce('&', res, s)
            return res
        elif node.tag == 'or':
            subs = [self.parse(x) for x in node.getchildren()]
            res = 0
            for i, s in enumerate(subs):
                if i == 0:
                    res = s
                else:
                    res = self.dag.make_reduce('|', res, s)
            return res
        elif node.tag == 'globally':
            sub = self.parse(node.getchildren()[0])
            return self.dag.make_reduce('G', sub)
        elif node.tag == 'finally':
            sub = self.parse(node.getchildren()[0])
            return self.dag.make_reduce('F', sub)
        elif node.tag == 'until':
            lhs = self.parse(next(node.iterchildren('before')).getchildren()[0])
            rhs = self.parse(next(node.iterchildren('reach')).getchildren()[0])
            return self.dag.make_reduce('U', lhs, rhs)
        elif node.tag == 'next':
            sub = self.parse(node.getchildren()[0])
            return self.dag.make_reduce('X', sub)
        elif node.tag == 'is-fireable':
            subs = tuple(self.net.t_ids[x.text] for x in node.getchildren() if x.tag == 'transition')
            p = IsFireable(self.net, *subs)
            self.props.append(p)
            return self.dag.make('p'+str(len(self.props)-1))
        elif node.tag == 'integer-le':
            lhs = self.parse(node.getchildren()[0])
            rhs = self.parse(node.getchildren()[1])
            p = IntegerLE(self.net, lhs, rhs)
            self.props.append(p)
            return self.dag.make('p'+str(len(self.props)-1))
        elif node.tag == 'integer-constant':
            return IntegerConstant(self.net, int(node.text))
        elif node.tag == 'integer-sum':
            subs = [self.parse(x) for x in node.getchildren()]
            return IntegerSum(self.net, *subs)
        elif node.tag == 'integer-difference':
            lhs = self.parse(node.getchildren()[0])
            rhs = self.parse(node.getchildren()[1])
            return IntegerDifference(self.net, lhs, rhs)
        elif node.tag == 'tokens-count':
            subs = tuple(self.net.p_ids[x.text] for x in node.getchildren() if x.tag == 'place')
            return TokensCount(self.net, *subs)


# Given a Petri net and a filename, parse the XML file and return the properties
# Each properties is a tuple of three parts: (id, ap, ltl)
def parse_property_xml(net, path):
    parser = etree.XMLParser(remove_comments=True, ns_clean=True)
    tree = etree.parse(path, parser=parser)

    # Strip annoying namespace tags
    for elem in tree.getiterator():
        elem.tag = etree.QName(elem).localname
    etree.cleanup_namespaces(tree)

    root = tree.getroot()
    props = []  # list for parsed PetriNet objects

    for prop_node in root.iter('property'):
        prop_id = ''
        prop = None
        for child in prop_node.getchildren():
            if child.tag == 'id':
                prop_id = child.text
            if child.tag == 'formula':
                prop = PropertyXMLParser(net)(child)
        props.append((prop_id, prop[0], prop[1]))
    return props
