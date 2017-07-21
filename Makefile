all: \
	O2_ts_1_david.jpg           \
	O2_ts_2_david_separate.jpg  \
  O2_ts_3_lazylet.jpg         \
  O2_ts_4_david2.jpg          \
  O2_ts_5_david2_separate.jpg \
  O2_ts_6_explicit.jpg        \
  O2_ts_7_lazyst.jpg          \
	O2_tl_1_david.jpg           \
	O2_tl_2_david_separate.jpg  \
  O2_tl_3_lazylet.jpg         \
  O2_tl_4_david2.jpg          \
  O2_tl_5_david2_separate.jpg \
  O2_tl_6_explicit.jpg        \
  O2_tl_7_lazyst.jpg          \
	O0_ts_1_david.jpg           \
	O0_ts_2_david_separate.jpg  \
  O0_ts_3_lazylet.jpg         \
  O0_ts_4_david2.jpg          \
  O0_ts_5_david2_separate.jpg \
  O0_ts_6_explicit.jpg        \
  O0_ts_7_lazyst.jpg          \
	O0_tl_1_david.jpg           \
	O0_tl_2_david_separate.jpg  \
  O0_tl_3_lazylet.jpg         \
  O0_tl_4_david2.jpg          \
  O0_tl_5_david2_separate.jpg \
  O0_tl_6_explicit.jpg        \
  O0_tl_7_lazyst.jpg

O2_%.jpg:
	ghc -Wall -Werror -O2 -fforce-recomp -prof -DVariant_$(subst O2_,,$(subst .jpg,,$@)) TestProgress
	./TestProgress +RTS -hy
	mv TestProgress.hp $(subst jpg,hp,$@)
	hp2ps -c $(subst jpg,hp,$@)
	convert $(subst jpg,ps,$@) -rotate -90 -trim $@

O0_%.jpg:
	ghc -Wall -Werror -O0 -fforce-recomp -prof -DVariant_$(subst O0_,,$(subst .jpg,,$@)) TestProgress
	./TestProgress +RTS -hy
	mv TestProgress.hp $(subst jpg,hp,$@)
	hp2ps -c $(subst jpg,hp,$@)
	convert $(subst jpg,ps,$@) -rotate -90 -trim $@

.PHONY: clean
clean:
	rm -f TestProgress *.hi *.aux *.ps *.o *.hp *.jpg
