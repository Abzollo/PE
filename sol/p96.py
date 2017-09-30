import sys
from heapq import *

def BacktrackingSearch(constraints, domains):
    # make a heap (number of variables in domain, variable)
    unassigned = []
    for var in domains.iterkeys():
        heappush(unassigned, (len(domains[var]), var))
    # Start with empty assignments
    assignment = {}
    return Backtrack(constraints, domains, assignment, unassigned)

def Backtrack(constraints, domains, assignment, unassigned):
    # If we assigned all variables, return assignments
    if len(assignment) == 81: return assignment
    # Find the unassigned variable with the least domain values remaining
    _, var = heappop(unassigned)
    while var in assignment:
        _, var = heappop(unassigned)
    # Make assignments and forward checking
    for value in domains[var]:
        # Inference, if successful, add to assignments, and update affected unassigned too
        assignment[var] = value
        inferences = {}
        for neighbor in constraints[var].iterkeys():
            if value in domains[neighbor] and neighbor not in assignment:
                domains[neighbor].remove(value)
                heappush(unassigned, (len(domains[neighbor]), neighbor))
                inferences[neighbor] = value

        # then backtrack recursively, return result if successful
        result = Backtrack(constraints, domains, assignment, unassigned)
        if result: return result
        # Remove assignment and restore inferences
        del assignment[var]
        for (neighbor, old_value) in inferences.iteritems():
            domains[neighbor].add(old_value)

    return {}  # We couldn't find a full assignment if we reached this point

def AC3(G, D, verbose=False):
    """
    G is the graph of the constraints (all binary constraints),
    D is a dictionary of variable and their domains so far.
    """
    # Extract all arcs from the graph
    arcs = set((x1, x2) for x1 in G.iterkeys() for x2 in G[x1].iterkeys())
    if verbose:
        print "Arcs initially:\n", arcs
        print "Domains initially:\n", D, "\n"
    while arcs:
        (x1,x2) = arcs.pop()  # Remove an arbitrary arc
        if verbose: print "arc:", x1, x2
        if Revise(D, x1, x2, verbose):
            # We found some unsatisfiable constraints
            if len(D[x1]) == 0:
                if verbose:
                    print "\nUnsatisfiable CSP.", x1, "can't be assigned any value.\n"
                return False
            # Add all of x1 incoming arcs, except (x2,x1)
            arcs.update((xk,x1) for xk in G[x1].iterkeys() if xk != x2)
            if verbose:
                print "Arcs after update:\n", arcs
                print "Domains after revision:\n", D, "\n"
    # We checked the consistency of all of the arcs and none are unsatisfiable
    if verbose:
        print "\nArc consistency done. The domains at the end are:"
        for x in D:
            val = D[x].pop()
            print x, val
            D[x].add(val)
    return True

def Revise(D, x1, x2, verbose):
    # We revise this arc, so we update the arcs in AC3 and check if domain became empty
    revised = False
    if verbose: print "revising..."
    # Since the domain might change during iteration, we loop over a copy of it
    for val1 in D[x1].copy():
        # If we can find any color in D[x2] that is different from val1, then we're good
        if any(map(lambda val2 : val1 != val2, D[x2])):
            continue
        # Else we can't assign val1 to x1 at all, so remove it from the domain of x1
        else:
            if verbose:
                print "can't make arc from", x1, "with value", val1, "to any value in", D[x2]
            D[x1].remove(val1)
            revised = True
    return revised

def getSameColumn(coord):
    for i in xrange(1,10):
        if i != coord[0]: yield (i,coord[1])

def getSameRow(coord):
    for j in xrange(1,10):
        if j != coord[1]: yield (coord[0], j)

def getSameBlock(coord):
    iBlock = (coord[0]-1) / 3
    jBlock = (coord[1]-1) / 3
    for i in range(iBlock*3+1, iBlock*3+4):
        for j in range(jBlock*3+1, jBlock*3+4):
            if (i,j) != coord: yield (i,j)

def makeSudokuGraph():
    G = {(i,j): {}  for i in xrange(1,10) for j in xrange(1,10)}
    for coord in G.iterkeys():
        for neighbor in getSameColumn(coord):
            G[coord][neighbor] = 1
        for neighbor in getSameRow(coord):
            G[coord][neighbor] = 1
        for neighbor in getSameBlock(coord):
            G[coord][neighbor] = 1
    return G

def printSudokuResult(assignment):
    for i in xrange(1,10):
        print " ".join(str(assignment[(i,j)]) for j in xrange(1,10))
    print ""

#### Main program here ####
graph = makeSudokuGraph()
lines = open("p096_sudoku.txt").read().splitlines()
num_of_grids = len(lines) / 10
sol = 0
for grid in xrange(num_of_grids):
    # The domain initially is just all numbers
    domains = {}
    for i in range(1,10):
        for j in range(1,10):
            num = int(lines[grid*10+i][j-1])
            if num > 0: domains[(i,j)] = set([num])
            else: domains[(i,j)] = set(xrange(1,10))

    # Solve sudoku
    #AC3(graph, domains)  # Actually it makes the code slower :(
    assignment = BacktrackingSearch(graph, domains)
    printSudokuResult(assignment)
    sol += 100*assignment[(1,1)] + 10*assignment[(1,2)] + assignment[(1,3)]

print "Solution:", sol



