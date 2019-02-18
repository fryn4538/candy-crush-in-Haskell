# Grupp 69 är den bästa gruppen

Struktur

En lista med n antal listor med n antal element.

Detta är listan som visas för spelaren. 
(LODRÄT) så att ifall El 4 blir borttaget från listan så flyttas El 1-3 ner ett steg 
och ett slumpat element stoppas in först i listan (Längst upp)

[

  [El 1	  [El 1   [El 1   [El 1
   
   El 2    El 2    El 2    El 2
   
   El 3    El 3    El 3    El 3
   
   El 4]   El 4]   El 4]   El 4]
   
   								 ]
   								
Ännu en lista med n antal listor.
(VÅGRÄT) Så att vi kan för varje spelardrag kolla ifall 3 eller fler av samma element är
bredvid varandra både vågrätt och lodrätt. Denna lista kommer dock inte att fyllas på med 
slumpade element. 
Kanske kan vi för varje "GameLoop" få denna lista att tömmas och fyllas med elementen i 
vår displaylista så att den alltid stämmer utan att vi behöver stoppa in element på 
specifika platser. --Lång time complexity dock.

[

 [ El 1,   El 2,   El 3,   El 4 ]
 
 [ El 1,   El 2,   El 3,   El 4 ]

 [ El 1,   El 2,   El 3,   El 4 ]

 [ El 1,   El 2,   El 3,   El 4 ]

							     ]
							
Sätt att upptäcka 3 eller fler element i rad på:
Koden kollar element 1. 
IF EL n == El (n+1) THEN Counter +1 --Denna arbetar rekursivt
ELSE
	IF Counter >= 3
		THEN remove EL n, El (n+1), El (n+2) osv upp till n+x == counter.
		
		
type Position = (Int, Int)
		

