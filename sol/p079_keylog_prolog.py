with open("p079_keylog.txt") as f:
	keylog_array = str([[int(digit) for digit in code] for code in f.read().splitlines()])
	open("p079_keylog_prolog.txt", "wb").write(keylog_array)
	