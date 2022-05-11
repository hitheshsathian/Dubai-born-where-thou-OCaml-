import functools
from pydoc import resolve
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
		s1 = set()
		if isinstance(t, Variable):
			s1.add(t)
		elif isinstance(t, Function):
			for i in t.terms:
				s1 = s1.union(self.variables_of_term(i))
		return s1


	def variables_of_clause (self, c : Rule) -> set :
		s1 = set()
		s2 = set()
		s1 = self.variables_of_term(c.head)
		s2 = self.variables_of_term(c.body)

		s1 = s1.union(s2)
		return s1


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
		s1 = set()
		if isinstance(t, Function):
			new_terms = []
			for term in t.terms:
				if term in s.keys():
					new_terms.append(s[term])
				else:
					new_terms.append(term)
			return Function(t.relation, new_terms)
		elif t in s.keys():
			s1 = s[t]
		else:
			return t
		return s1


	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		s1 = set()
		s2 = set()
		if isinstance(c.head, Function):
			s1 = self.substitute_in_term(s, c.head) 
			return Rule(s1, c.body)
		
		if isinstance(c.body, Function):
			s2 = self.substitute_in_term(c.body)
		elif isinstance(c.body.terms, Variable):
			s2 = Function(c.body.relation, s[c.body])
		return Rule(s1, RuleBody(s2))
		

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
		#unify(X,Y,𝜃) = 
 		#X = X𝜃
 		#Y = Y𝜃
		def unify_helper(t1: Term, t2: Term, unifier: dict):
			X = self.substitute_in_term(unifier, t1)
			Y = self.substitute_in_term(unifier, t2)

			#X is a variable that does not occur in Y:
   			#return (𝜃{X/Y} ∪ {X/Y}) /*replace X with Y in the substitution terms of 𝜃 add X/Y to 𝜃*/
			if isinstance(X, Variable) and self.occurs_check(X, Y) == False:
				for i in unifier.keys():
					unifier[i] = self.substitute_in_term({X : Y}, unifier[i])
				unifier[X] = Y
				return unifier
			#Y is a variable that does not occur in X:
   			#return (𝜃{Y/X} ∪ {Y/X}) /*replace Y with X in the substitution terms of 𝜃 add Y/X to 𝜃*/
			elif isinstance(Y, Variable) and self.occurs_check(Y, X) == False:
				for i in unifier.keys():
					unifier[i] = self.substitute_in_term({Y : X}, unifier[i])
				unifier[Y] = X
				return unifier
			#X and Y are indentical constants or variables:
   			#return 𝜃
			if (X == Y):
				return unifier

			# if X is f(X1,...,Xn) and Y is f(Y1,...,Yn):
   			#return (fold_left (fun 𝜃 (X,Y) -> unify(X,Y,𝜃)) 𝜃 [(X1,Y1),...,(Xn,Yn)])
			if isinstance(X, Function) and isinstance(Y, Function):
				tuple_of_terms = zip(X.terms, Y.terms)
				for x,y in tuple_of_terms:
					unifier = unify_helper(x,y,unifier)
				return unifier
			#otherwise:
   			#raise FAIL
			else:
				raise Not_unifiable()
		#let unify(X,Y) = unify(X,Y,ϵ)
		return unify_helper(t1, t2, {})

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
		#G = Goal
		G = pgoal[0: len(pgoal)]
		#Initialise resolvent to G.
		resolvent = pgoal[0: len(pgoal)]
		while True:
			#while (the resolvent is not empty)
			while resolvent:
				#choose a goal A from the resolvent //random goal
				random_index_res = random.randint(0, len(resolvent) - 1)
				A = resolvent[random_index_res]
				#choose a (renamed) clause A' <- B1,...,Bn from P 
				random_index_prog = random.randint(0, len(program) -1)
				A_prime = self.freshen(program[random_index_prog])
				#such that A and A' unify with a unifier 𝜃 // random rule
				#(if no such goal and clause exist, exit the while loop).
				try:
					unifier = self.unify(A,A_prime.head)
				except:
					break
				#replace A by B1,...,Bn in the resolvent
				resolvent.remove(A)
				for term in A_prime.body.terms:
					resolvent.append(term)
				
				#apply 𝜃 to the resolvent and G
				for term in resolvent:
					resolvent = [self.substitute_in_term(unifier, term)]

				for term in G:
					G = [self.substitute_in_term(unifier, term)]

			#If the resolvent is empty, then output G, else goto L (nondet_query).
			if not resolvent:
				return G
			else:
				continue
			


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
