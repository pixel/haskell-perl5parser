Perl5Parser_HS = Perl5Parser/Common.hs Perl5Parser/Types.hs Perl5Parser/Serialize.hs Perl5Parser/ParserHelper.hs Perl5Parser/Prototype.hs Perl5Parser/Token/Number.hs Perl5Parser/Token/Quote.hs Perl5Parser/Token/QuoteLike.hs Perl5Parser/Token/Regexp.hs Perl5Parser/Token/HereDoc.hs Perl5Parser/Token.hs Perl5Parser/Term.hs Perl5Parser/Expr.hs Perl5Parser/Lines.hs Perl5Parser/Document.hs

Perl5Parser_O = $(Perl5Parser_HS:%.hs=%.o)

all: TAGS test
	./test /tmp/t.pl && diff -u /tmp/t.pl /tmp/t.pl.new

test: $(Perl5Parser_O) test.hs
	ghc -W $^ -o $@ -package parsec

%.o: %.hs
	ghc -W -c $<

%.o-boot: %.hs-boot
	ghc -W -c $<

clean:
	find -name "*.o" | xargs rm -f
	find -name "*.hi" | xargs rm -f
	find -name "*.o-boot" | xargs rm -f
	find -name "*.hi-boot" | xargs rm -f
	rm -f TAGS test

TAGS: $(Perl5Parser_HS)
	hasktags -e $^


Perl5Parser/Term.o: Perl5Parser/Expr.o-boot Perl5Parser/Lines.o-boot
Perl5Parser/Expr.o: Perl5Parser/Lines.o-boot
