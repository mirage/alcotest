all:
	jbuilder build @install @DEFAULT --dev

clean:
	jbuilder clean

.PHONY: all clean
