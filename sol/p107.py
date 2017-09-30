import sys

### Nodes and edges constructors ###
class Node:
    def __init__ (self, label):
        self.label = label
    def __str__(self):
        return self.label

def make_edge(G, node1, node2, cost, undirected=False):
    if node1 not in G: G[node1] = {}
    if node2 not in G: G[node2] = {}
    G[node1][node2] = cost
    if undirected: G[node2][node1] = cost

### Union-Find functions ###
def MakeSet(x):
     x.parent = x
     x.rank   = 0

def Union(x, y):
     xRoot = Find(x)
     yRoot = Find(y)
     if xRoot.rank > yRoot.rank:
         yRoot.parent = xRoot
     elif xRoot.rank < yRoot.rank:
         xRoot.parent = yRoot
     elif xRoot != yRoot: # Unless x and y are already in same set, merge them
         yRoot.parent = xRoot
         xRoot.rank = xRoot.rank + 1

def Find(x):
     if x.parent == x:
        return x
     else:
        x.parent = Find(x.parent)
        return x.parent

### Kruskal's algorithm ###
def kruskal(G):
    # First make the disjoint sets
    sets = {}
    for v in G.keys():
        node = Node(v)
        MakeSet(node)
        sets[v] = node
    # Now find all edges with their costs
    edges = {(G[u][v], tuple(sorted((u, v))) ) for u in G.keys() for v in G[u].keys()}
    #
    A = set()
    for c, edge in sorted(edges):
        u, v = edge
        u = sets[u]
        v = sets[v]
        if Find(u) != Find(v):
            A |= {(u, v)}
            Union(u, v)
    return [(u.label, v.label) for u, v in A]


#################
####  START  ####
#################

# Read input
lines = []
with open("p107_network.txt") as f:
    lines.extend(f.read().splitlines())

# Build graph
G = {}
for i in range(len(lines)):
    costs = lines[i].split(",")
    for j in range(len(costs)):
        if costs[j] != "-":
            make_edge(G, i, j, int(costs[j]))

all_edges_costs = sum(G[u][v] for u in G.keys() for v in G[u].keys()) // 2
msp_edges_costs = sum(G[u][v] for u, v in kruskal(G))

print(all_edges_costs - msp_edges_costs)
