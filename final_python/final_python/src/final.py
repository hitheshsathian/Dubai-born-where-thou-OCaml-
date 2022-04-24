from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		return set()

	def variables_of_clause (self, c : Rule) -> set :
		return set()


	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		return c


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unify (self, t1: Term, t2: Term) -> dict:
		return {}


	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		return []


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		return [pgoal]
