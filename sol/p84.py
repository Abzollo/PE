# Answer is 10 15 24

import random

def nextR(pos):
	if pos == 7: return 15
	if pos == 22: return 25
	return 5

def nextU(pos):
	if pos == 22: return 28
	return 12

# Seed
random.seed()

# [GO, JAIL, nothing...]
CC = [0, 10] + [-1]*14
# [GO, JAIL, C1, E3, H2, R1, nextR, nextR, next U, back 3 squares, nothing...]
CH = [0, 10, 11, 24, 39, 5, -10, -10, -100, -1000] + [-1]*6

# Shuffle cards
random.shuffle(CC)
random.shuffle(CH)
# Start at 0 for each deck
CCptr = CHptr = 0
# Specify special blocks locations on board
CCblocks = [2, 17, 33]
CHblocks = [7, 22, 36]

# Start game
turns = 10**6
posHistogram = [0]*40
pos = 0
doubles = 0
r = random.SystemRandom()
for _ in range(turns):
	# Throw dice and check for doubles
	die1 = random.randint(1,4)
	die2 = random.randint(1,4)
	if die1 == die2: doubles += 1
	else: doubles = 0
	steps = die1 + die2
	# Move piece
	prevPos = pos
	pos = (pos + steps) % 40
	# Check if we should GO 2 JAIL
	if doubles == 3 or pos == 30:
		pos = 10
		doubles %= 3  # This will reset doubles when it's 3
	# Check if on a CH block
	if pos in CHblocks:
		if CH[CHptr] == -1: pass
		elif CH[CHptr] == -10:
			pos = nextR(pos)
		elif CH[CHptr] == -100:
			pos = nextU(pos)
		elif CH[CHptr] == -1000:
			pos -= 3
		else:
			pos = CH[CHptr]
		CHptr = (CHptr + 1) % 16
	# Check if on a CC block
	if pos in CCblocks:
		if CC[CCptr] != -1:
			pos = CC[CCptr]
		CCptr = (CCptr + 1) % 16
	
	# We reached a stable position
	posHistogram[pos] += 1

[print("Square "+str(i)+":", str(100*p/turns)+"%") for p, i in sorted(zip(posHistogram, range(40)), reverse=True)]
		
		
		
		