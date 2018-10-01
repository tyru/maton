
maton: maton.pl
	swipl -g main -o maton -c maton.pl

test:
	swipl test.pl

.PHONY: test
