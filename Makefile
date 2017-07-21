all: \
	O2-ts-1-david-.jpg           \
	O2-ts-2-david_separate-.jpg  \
  O2-ts-3-lazylet-.jpg         \
  O2-ts-4-david2-.jpg          \
  O2-ts-5-david2_separate-.jpg \
  O2-ts-6-explicit-.jpg        \
  O2-ts-7-lazyst-.jpg          \
	O2-tl-1-david-.jpg           \
	O2-tl-2-david_separate-.jpg  \
  O2-tl-3-lazylet-.jpg         \
  O2-tl-4-david2-.jpg          \
  O2-tl-5-david2_separate-.jpg \
  O2-tl-6-explicit-.jpg        \
  O2-tl-7-lazyst-.jpg          \
	O0-ts-1-david-.jpg           \
	O0-ts-2-david_separate-.jpg  \
  O0-ts-3-lazylet-.jpg         \
  O0-ts-4-david2-.jpg          \
  O0-ts-5-david2_separate-.jpg \
  O0-ts-6-explicit-.jpg        \
  O0-ts-7-lazyst-.jpg          \
	O0-tl-1-david-.jpg           \
	O0-tl-2-david_separate-.jpg  \
  O0-tl-3-lazylet-.jpg         \
  O0-tl-4-david2-.jpg          \
  O0-tl-5-david2_separate-.jpg \
  O0-tl-6-explicit-.jpg        \
  O0-tl-7-lazyst-.jpg

GHC=ghc -Wall -Werror -fforce-recomp -prof

%.jpg:
	${GHC}      -$(word 1, $(subst -, ,$@)) \
	       -DTop_$(word 2, $(subst -, ,$@)) \
				 -DAlg_$(word 4, $(subst -, ,$@)) \
				 TestProgress
	./TestProgress +RTS -hy
	mv TestProgress.hp $(subst jpg,hp,$@)
	hp2ps -c $(subst jpg,hp,$@)
	convert $(subst jpg,ps,$@) -rotate -90 -trim $@

.PHONY: clean
clean:
	rm -f TestProgress *.hi *.aux *.ps *.o *.hp *.jpg
