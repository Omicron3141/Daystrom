Compile : DaystromSyntax.hs DaystromParser.hs Daystrom.hs
	ghc -o Daystrom $^

clean:
	rm -f *.o *.hi
