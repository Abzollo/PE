# It's tedious and not straightforward to code shortest path algorithm in Haskell,
# so I solved this problem in python

import sys
from collections import deque
from heapq import *

def makeEdge(G, node1, node2, cost):
	# Assuming graphs are given undirected
	if node1 not in G: G[node1] = {}
	G[node1][node2] = cost

def UCS(G, start, goal):
	parents = {(i,j) : None for i in xrange(80) for j in xrange(80)}
	parents[(-1,-1)] = parents[(80,80)] = None
	distances = {(i,j) : float("inf") for i in xrange(80) for j in xrange(80)}
	distances[(-1,-1)] = distances[(80,80)] = float("inf")
	explored = {(i,j) : False for i in xrange(80) for j in xrange(80)}
	explored[(-1,-1)] = explored[(80,80)] = False
	
	if goal == start: return "0"
	distances[start] = 0
	frontier = [(0, start)]
	while frontier:
		dist, node = heappop(frontier)  # Frontier is a heap in UCS
		if goal == node: return str(dist)
		explored[node] = True  # Node is explored in UCS after it's popped from frontier
		# Expand nodes
		for child in G[node].iterkeys():
			childDist = dist + G[node][child]
			if explored[child]: continue  # Skip since it's already explored
			# Update distance if it's smaller than current distance
			if childDist < distances[child]:
				distances[child] = childDist
				parents[child] = node
			heappush(frontier, (childDist, child))
	# Could not find a feasible path to the goal node
	return "No Path"

def nbhr(i,j):
	if i > 0: yield (i-1,j)
	if i < 79: yield (i+1,j)
	if j > 0: yield (i, j-1)
	if j < 79: yield (i, j+1)

M = [[int(x) for x in line.split(",")] for line in open("p083_matrix.txt").read().splitlines()]
# Build Graph of matrix
G = {}
for i in xrange(80):
	for j in xrange(80):
		for m, n in nbhr(i,j):
			makeEdge(G,(i,j), (m,n), M[m][n])
# Add source and sink nodes and connect them to the matrix left and right sides
makeEdge(G, (-1,-1), (0,0), M[0][0])
makeEdge(G, (79,79), (80,80), 0)
print UCS(G,(-1,-1),(80,80))


