Copyright (c) 1985, 1994 Free Software Foundation, Inc.  Szczeg�y na
ko�cu pliku.

Czytasz w�a�nie kr�tki podr�cznik Emacsa.

Polecenia Emacsa og�lnie wymagaj� wci�ni�cia klawisza CONTROL (czasami
oznaczanego CTRL lub CTL) lub klawisza META.  Na niekt�rych
klawiaturach klawisz META jest oznaczany ALT lub EDIT, albo jeszcze
inaczej (na przyk�ad na klawiaturach firmy SUN klawisz ze znakiem karo
na lewo od klawisza odst�pu to jest w�a�nie META).  Je�li nie masz
klawisza META, mo�esz w jego zast�pstwie u�ywa� ESC.  Zamiast pisa�
META czy CONTROL za ka�dym razem, gdy masz przycisn�� ten klawisz,
u�ywa� b�dziemy nast�puj�cych skr�t�w:

 C-<znak> oznacza trzymanie klawisza CONTROL podczas
          wciskania klawisza <znak>.  Na przyk�ad C-f b�dzie odpowiada�o
          naci�ni�ciu f, podczas gdy klawisz CONTROL by� wci�ni�ty.

 M-<znak> oznacza trzymanie klawisza META wci�ni�tego podczas
          wciskania klawisza <znak>.  Je�li nie masz klawisza META,
          naci�nij i pu�� klawisz ESC, a potem naci�nij
          klawisz <znak>.

Uwaga: by zako�czy� sesje Emacsa naci�nij C-x C-c (dwa znaki).

Znaki ">>" na lewym marginesie w dalszej cz�ci tego podr�cznika
oznaczaj� �wiczenia dla Ciebie.  Na przyk�ad: 
<<Blank lines inserted here by startup of help-with-tutorial>>

>>  Teraz naci�nij C-v (nast�pny ekran), by przej�� na nast�pny ekran
    podr�cznika (zr�b to naciskaj�c jednocze�nie klawisz CONTROL
    i v).  Od tego momentu powiniene� robi� to zawsze, gdy dojdziesz
    do ko�ca ekranu.

Zwr�� uwag� na to, ze kilka linii powtarza si�, gdy przechodzisz z
ekranu na ekran; zachowanie to ma zapewni� pewna ci�g�o�� podczas
przesuwania si� w obr�bie pliku.

Pierwsza umiej�tno�ci�, kt�ra powiniene� opanowa�, jest spos�b
przesuwania si� z miejsca na miejsce.  Ju� wiesz, jak przesuwa� si� o
jeden ekran do przodu.  By przesun�� si� o jeden ekran do tylu,
wci�nij M-v (wci�nij META i naci�nij v, lub naci�nij <ESC>v je�li nie
masz klawisza META lub EDIT).

>>  Spr�buj nacisn�� M-v, a potem C-v by przesun�� si� w prz�d i w ty�
    kilka razy.


PODSUMOWANIE
------------

Nast�puj�ce polecenia s� u�yteczne do przegl�dania po jednym ekranie:

	C-v	Przesu� si� o jeden ekran do przodu
	M-v	Przesu� si� o jeden ekran do tylu
	C-l	Wyczy�� ekran i wy�wietl go na nowo, umieszczaj�c
                tekst z okolic kursora w �rodku ekranu.
                (Ta kombinacja to CONTROL-L, a nie CONTROL-1.)

>> Znajd� kursor i zapami�taj, jaki tekst jest w jego okolicy.
   Naci�nij nast�pnie C-l.
   Znajd� kursor jeszcze raz i zwr�� uwag�, �e znajduje 
   si� on w okolicy tego samego tekstu.


PODSTAWY KIEROWANIA KURSOREM
----------------------------

Przesuwanie si� z ekranu na ekran jest u�yteczne, ale jak przej�� do
okre�lonego miejsca w obr�bie jednego ekranu?

Mo�na to zrobi� na kilka sposob�w.  Najprostszym jest u�ycie polece�
C-p, C-b, C-f oraz C-n.  Ka�de z tych polece� przesuwa kursor o jeden
wiersz lub kolumn� w okre�lonym kierunku.  Oto schemat, kt�ry to
obrazuje:

	           Poprzednia linia, C-p
                   (ang. previous line)
                            :
                            :
       Wstecz, C-b ....  Kursor ....  Do przodu, C-f
       (ang. back)          :         (ang. forward)    
                            :
                            :
                    Nast�pna linia, C-n
                     (ang. next line)

>> Przesu� kursor na �rodek schematu za pomoc� C-n lub C-p.  Naci�nij
   potem C-l, by zobaczy� ca�y diagram na �rodku ekranu.

To s� podstawowe polecenia kieruj�ce po�o�eniem kursora, kt�rych
b�dziesz u�ywa� nieustannnie, warto wi�c je zapami�ta�.

>> Naci�nij kilka razy C-n, by przesun�� kursor do tej linii.

>> Przesu� si� w g��b linii za pomoc� C-f, a potem do g�ry za pomoc�
   C-p.  Zwr�� uwag� na zachowanie si� C-p, gdy kursor jest w �rodku
   linii.

Ka�da linia tekstu ko�czy si� znakiem nowej linii, kt�ry oddziela ja
od nast�pnej.  Ka�dy Tw�j plik powinien ko�czy� si� znakiem nowej
linii (ale Emacs nie zmusza Ci� do tego).

>> Spr�buj nacisn�� C-b na pocz�tku linii.  Powinno to Ci� przenie��
   na koniec poprzedniej linii.  Dzieje si� tak dlatego, �e kursor
   przechodzi nad znakiem nowej linii.

C-f przechodzi nad znakiem nowej linii tak samo jak C-b.

>> Naci�nij kilka razy C-b, by� dostrzeg�, gdzie jest kursor.
   Naci�nij potem C-f, by wr�ci� na koniec linii.  W ko�cu naci�nij
   jeszcze raz C-f, by przej�� do nast�pnej linii.

Gdy przesuwasz kursor poza dolna kraw�d� ekranu, tekst za kraw�dzi�
przesuwa si� na ekran (ang. scrolling).  Dzi�ki temu Emacs mo�e
przesun�� kursor do okre�lonego miejsca bez umieszczania go poza
ekranem.

>> Spr�buj przesun�� kursor poza dolna granice ekranu za pomoc� C-n i
   zobacz co si� stanie.

Je�li przesuwanie si� o jeden znak na raz jest dla Ciebie za wolne,
spr�buj przesuwa� si� o s�owa.  M-f (Meta-f) przesuwa kursor do przodu
o s�owo, a M-b przesuwa go do tylu o jedno s�owo.

>> Spr�buj nacisn�� kilka M-f i M-b.

Gdy jeste� w �rodku s�owa, M-f przesuwa kursor na koniec s�owa.  Je�li
natomiast jeste� w przerwie miedzy s�owami, M-f przesuwa kursor na
koniec nast�pnego s�owa.  M-b zachowuje si� podobnie, jak chodzi o
ruch do ty�u.

>> Naci�nij M-f i M-b kilka razy na przemian z C-f i C-b tak, by�
   m�g� zauwa�y� dzia�anie M-f i M-b naci�nietych w r�nych miejscach
   wewn�trz i pomi�dzy s�owami.

Zauwa� podobie�stwo pomi�dzy C-f i C-b oraz M-f i M-b.  Bardzo cz�sto
kombinacje zawieraj�ce Meta opisuj� operacje zwi�zane z jednostkami
j�zykowymi (s�owa, zdania, akapity), podczas gdy kombinacje oparte o
Control dzia�aj� na podstawowych jednostkach niezale�nych od tego, co
edytujesz (znaki, linie, itd.).

Ta zale�no�� stosuje si� do linii i zda�: C-a i C-e przesuwaj� kursor
na pocz�tek i koniec linii, a M-a i M-e przesuwaj� go na pocz�tek i
koniec zdania.

>> Naci�nij kilka razy C-a, a potem kilka razy C-e.
   Powt�rz to z M-a, a potem z M-e.

Czy zauwa�y�e�, ze powtarzanie C-a nic nie zmienia, natomiast powt�rne
M-a przesuwa Ci� o jedno zdanie?  Chocia� nie jest to do ko�ca
analogiczne, wydaje si� jednak naturalne.

Po�o�enie kursora w tek�cie okre�lane jest mianem "punktu".

Oto podsumowanie prostych polece� s�u��cych do przesuwania kursora,
w��cznie z operacjami dotycz�cymi s��w i zda�:

	C-f	Do przodu o jeden znak
	C-b	W ty� o jeden znak

	M-f	Do przodu o s�owo
	M-b	W ty� o s�owo

	C-n	Nast�pna linia
	C-p	Poprzednia linia

	C-a	Pocz�tek linii
	C-e	Koniec linii

	M-a	W ty� na pocz�tek zdania
	M-e	Do przodu na koniec zdania

>> Prze�wicz kilka razy wszystkie powy�sze polecenia dla wprawy.
   S� one najcz�ciej u�ywanymi poleceniami.

Dwa inne wa�ne polecenia przesuwaj�ce kursor to M-< (Meta i znak
mniejszo�ci), kt�re przesuwa kursor na pocz�tek ca�ego tekstu i M->
(Meta i znak wi�kszo�ci), kt�re przesuwa kursor na koniec ca�ego
tekstu.

Na wi�kszo�ci terminali "<" jest nad przecinkiem, tak wiec musisz u�y�
klawisza Shift by nacisn�� "<".  Musisz wiec tak�e u�y� Shift by
nacisn�� M-<.  Bez Shift by�oby to M-przecinek.

>> Naci�nij M-< by przej�� na pocz�tek podr�cznika.  U�yj potem C-v
   kilkukrotnie, by wr�ci� tutaj.

>> Teraz naci�nij M->, by przej�� na koniec podr�cznika.  Wr�� do tego
   miejsca za pomoc� kilkukrotnego M-v.

Je�li Tw�j terminal ma klawisze strza�ek, to mo�esz ich u�y� do
przesuwania kursora.  Zalecamy Ci nauczenie si� kombinacji C-b, C-f,
C-n i C-p z trzech powod�w.  Po pierwsze, dzia�aj� one na wszystkich
typach terminali.  Po drugie, gdy ju� zdob�dziesz pewna praktyk� w
pos�ugiwaniu si� Emacsem, b�dzie Ci szybciej nacisn�� te kombinacje
ni� klawisze strza�ek (poniewa� nie wymaga to przenoszenia d�oni z
miejsca, kt�re zajmuj� podczas szybkiego pisania za pomoc� 10 palc�w).
Po trzecie wreszcie, gdy ju� wyrobisz sobie zwyczaj pos�ugiwania si�
tymi poleceniami z klawiszem Control, b�dziesz m�g� �atwo nauczy� si�
innych zaawansowanych polece� przesuwaj�cych kursor.

Wi�kszo�� polece� Emacsa akceptuje argument liczbowy; dla wi�kszo�ci
polece� s�u�y on jako liczba powt�rze�.  Spos�b, w jaki okre�lasz
liczb� powt�rze� polecenia, to naci�niecie C-u a potem cyfr, zanim
naci�niesz polecenie.  Je�li masz klawisz META (lub EDIT lub ALT),
alternatywnym sposobem jest wciskanie klawiszy cyfr podczas
wprowadzania argumentu liczbowego.  Zalecamy nauczenie si� metody
klawisza C-u, poniewa� dzia�a ona na wszystkich terminalach.

Na przyk�ad C-u 8 C-f przesuwa kursor do przodu o osiem znak�w.
	
>> Spr�buj u�y� C-n i C-p z argumentem liczbowym, by przesun�� kursor
   do jednej z linii w pobli�u tego zdania za pomoc� tylko jednego
   polecenia.

Wi�kszo�� polece� u�ywa argumentu liczbowego jako liczba powt�rze�.
Jest kilka polece�, kt�re u�ywaj� go w inny spos�b.  C-v i M-v s�
w�r�d tych wyj�tk�w.  Je�li poda si� im argument, przesuwaj� zawarto��
ekranu w gore lub w d� o podana liczb� linii zamiast o tyle� ekran�w.
Na przyk�ad C-u 4 C-v przewija ekran o 4 linie.

>> Spr�buj nacisn�� C-u 8 C-v.

To powinno by�o przewin�� ekran do g�ry o 8 linii.  Je�li chcia�by�
przewin�� go w d�, mo�esz poda� argument przed poleceniem M-v.

Je�li u�ywasz systemu X-Windows, prawdopodobnie po prawej stronie okna
Emacsa znajduje si� prostok�tny obszar, nazywany po angielsku
"scrollbar".  Za jego pomoc� mo�esz przewija� tekst, u�ywaj�c do tego
celu myszy.

>> Spr�buj nacisn�� �rodkowy klawisz myszy u g�ry pod�wietlonego
   obszaru na scrollbarze.  To powinno przewin�� tekst do miejsca
   okre�lonego wysoko�ci�, na kt�rej nacisn��e� klawisz myszy.

>> Przesu� mysz do miejsca oddalonego od g�rnego ko�ca scrollbaru
   mniej wi�cej o trzy linie i naci�nij lewy klawisz myszy kilka razy.


* KIEROWANIE KURSOREM Z X TERMINALA
-----------------------------------

Je�li masz X terminal, prawdopodobnie �atwiej Ci b�dzie u�ywa�
klawiszy strza�ek po prawej stronie klawiatury do kierowania kursorem.
Klawisze strza�ek w lewo, w prawo, w g�r� i w d� dzia�aj� zgodnie z
oczekiwaniem; odpowiadaj� one dok�adnie C-b, C-f, C-p i C-n, ale s�
�atwiejsze do zapami�tania.  Mo�esz tak�e u�ywa� C-lewo i C-prawo by
przesuwa� si� o s�owa oraz C-g�ra i C-d�, by przesuwa� si� o bloki
(np. akapity, je�li edytujesz tekst).  Je�li masz klawisze oznaczone
HOME (lub BEGIN) oraz END, zanios� Ci� one na pocz�tek i koniec linii,
a C-home i C-end na pocz�tek i koniec pliku.  Je�li Twoja klawiatura
ma klawisze PgUp i PgDn, mo�esz ich u�y� do przesuwania si� o jeden
ekran za jednym razem, tak jak M-v i C-v.

Wszystkie te polecenia akceptuj� argument liczbowy, jak to jest
opisane powy�ej.  Mo�esz stosowa� pewne skr�ty w celu wpisania tych
argument�w: naci�nij i trzymaj CONTROL lub META i wpisz liczb�.  Na
przyk�ad, by przesun�� kursor o 12 s��w w prawo naci�nij C-1 C-2
C-prawo.  Zwr�� uwag�, ze jest to �atwe do wpisania, poniewa� nie
musisz puszcza� klawisza CONTROL podczas wciskania klawiszy.


* GDY EMACS JEST ZABLOKOWANY
----------------------------

Je�li Emacs przestaje odpowiada� na Twoje polecenia, mo�esz go
bezpiecznie zatrzyma� przyciskaj�c C-g.  Mo�esz u�y� C-g do przerwania
polecenia, kt�re zabiera zbyt wiele czasu.

Mo�esz u�y� C-g tak�e, by anulowa� argument liczbowy lub pocz�tek
polecenia, kt�rego nie chcesz doka�cza�.

>> Napisz C-u 100 jako argument liczbowy, po czym naci�nij C-g.  
   Teraz naci�nij C-f.  Powinno przesun�� to kursor o tylko jeden
   znak, poniewa� anulowa�e� argument za pomoc� C-g.

Je�li nacisn��e� <ESC> przez pomy�k�, mo�esz tego si� pozby� za pomoc�
C-g.


* ZABLOKOWANE POLECENIA
-----------------------

Pewne polecenia Emacsa s� "zablokowane", tak by pocz�tkuj�cy
u�ytkownicy nie mogli ich wywo�a� przez przypadek.

Je�li wywo�asz jedno z zablokowanych polece�, Emacs wypisze komunikat
informuj�cy o tym, co to za polecenie, i zapyta Ci�, czy chcesz je
wywo�a�.

Je�li naprawd� chcesz wywo�a� to polecenie, naci�nij spacje w
odpowiedzi na pytanie.  Je�li nie chcesz wywo�a� zablokowanego
polecenia, odpowiedz na pytanie naciskaj�c "n".

>> Napisz `C-x n p' (co jest zablokowanym poleceniem) i odpowiedz "n"
   na zadane pytanie.


* OKNA
------

Emacs mo�e mi�� kilka okien, ka�de wy�wietlaj�ce w�asny tekst.  Zwr��
uwag�, ze "okno" je�li chodzi o Emacsa, nie odnosi si� do osobnego
okienka systemu okienkowego, ale do pojedynczego panelu wewn�trz
okienka systemu X-Windows.  (Emacs mo�e mi�� kilka X-okien, lub
"ramek" w terminologii Emacsa.  Opisane jest to poni�ej.)

Na tym etapie lepiej jest si� nie zag��bia� w techniki wykorzystuj�ce
kilka okien.  Powiniene� jedynie wiedzie�, w jaki spos�b pozby� si�
nadmiaru okien, kt�re mog� pojawi� si� jako sk�adniki systemu pomocy
lub wynik pewnych polece�.  Robi si� to w prosty spos�b:

	C-x 1	Jedno okno (tzn. zabij wszystkie inne okna).

Kombinacja ta to Control-x, po kt�rym wyst�puje cyfra 1.  C-x 1
powi�ksza okno, w kt�rym jest kursor tak, by zaj�o ca�y ekran.
Kasuje to wszystkie inne okna Emacsa.

>> Przesu� kursor do tej linii i naci�nij C-u 0 C-l.

(C-l, jak pami�tasz od�wie�a zawarto�� ekranu.  Je�li poda si� temu
poleceniu argument liczbowy, b�dzie to oznacza�o "od�wie� zawarto��
ekranu i umie�� bie��ca linie o tyle linii od g�ry ekranu".  Tak wiec
C-u 0 C-1 oznacza "od�wie� ekran, umieszczaj�c bie��ca linie na samej
g�rze".)

>> Naci�nij Control-x 2
   Zauwa� jak okno si� kurczy, podczas gdy nowe okno pojawia si�,
   wy�wietlaj�c zawarto�� tego bufora.

>> Naci�nij C-x 1 i nowe okno zniknie.


* WSTAWIANIE I USUWANIE
-----------------------

Je�li chcesz wstawia� tekst, po prostu go napisz.  Znaki, kt�re da si�
wy�wietli�, takie jak A, 7, *, itd, Emacs traktuje jako tekst i
wstawia natychmiast do bufora.  Naci�nij <Return> (znak powrotu
karetki), by wstawi� znak nowej linii.

Ostatni znak, kt�ry napisa�e� mo�esz skasowa� przez naci�niecie
klawisza <Delete>.  Klawisz ten mo�e by� oznaczony "Del".  W pewnych
wypadkach klawisz "Backspace" mo�e s�u�y� za <Delete>, ale nie jest to
regu��!

Og�lniej, <Delete> usuwa znak bezpo�rednio przed bie��ca pozycj�
kursora.

>> Zr�b to teraz: wstaw kilka znak�w, po czym usu� je za pomaca
   kilkukrotnego naci�ni�cia <Delete>.  Nie przejmuj si� tym, 
   �e zmieniasz ten plik; nie zmienisz w ten spos�b g��wnego pliku
   podr�cznika.  To jest Twoja w�asna kopia.

Gdy linia tekstu staje si� zbyt d�uga, by zmie�ci� si� w jednym
wierszu na ekranie, jest ona "kontynuowana" w nast�pnym wierszu
ekranu.  Znak "backslash" (`\') na prawym marginesie pozwala Ci
rozpozna� takie linie.

>> Wpisuj jaki� tekst tak d�ugo, a� dojdziesz do prawego marginesu, i
   potem nie przestawaj.  Zauwa�ysz, ze pojawi si� linia kontynuacji.

>> U�yj <Delete> by usun�� tekst tak, by linia znowu
   mie�ci�a si� na ekranie.  Linia kontynuacji zniknie.

Znak nowej linii mo�e by� kasowany tak, jak ka�dy inny znak.
Usuniecie znaku nowej linii ��czy je w jedna.  Je�li powsta�a w wyniku
tego linia jest zbyt d�uga, by zmie�ci� si� na szeroko�� ekranu,
zostanie ona wy�wietlona z lini� kontynuacji.

>> Przesu� kursor na pocz�tek linii i naci�nij <Delete>.  Bie��ca
   linia zostanie po��czona z poprzednia.

>> Naci�nij <Return>, by z powrotem wstawi� znak nowej linii, kt�ry
   skasowa�e�.

Pami�taj, ze wi�kszo�� polece� Emacsa mo�e zosta� wywo�anych z
parametrem liczby powt�rze�; dotyczy to tak�e znak�w tekstu.  Argument
liczbowy powoduje wstawienie znaku kilkukrotnie.

>>  Spr�buj zrobi� to teraz -- naci�nij C-u 8 * by uzyska� ********.

Nauczy�e� si� ju� wi�kszej cz�ci podstawowych sposob�w pisania oraz
poprawiania b��d�w.  W Emacsie mo�esz usuwa� r�wnie� cale s�owa lub
cale linie.  Oto podsumowanie operacji usuwania znak�w:

	<Delete>     usu� znak bezpo�rednio przed kursorem
	C-d          usu� znak bezpo�rednio za kursorem

	M-<Delete>   wytnij s�owo bezpo�rednio przed kursorem
	M-d          wytnij nast�pne s�owo bezpo�rednio za kursorem

	C-k          wytnij zawarto�� linii od kursora do jej ko�ca
	M-k          wytnij wszystkie znaki od kursora do ko�ca zdania

Zauwa�, ze <Delete> i C-d w po��czeniu z M-<Delete> i M-d rozszerzaj�
regule rozpocz�t� przez C-f i M-f (C�, <Delete> tak naprawd� nie
wymaga wci�ni�cia Control, ale pomi�my to milczeniem).  C-k i M-k s�
podobne do C-e i M-e w sensie, �e linie s� odpowiednikami zda�.

Gdy usuwasz wi�cej ni� jeden znak naraz, Emacs zachowuje usuni�ty
tekst tak, by� m�g� go gdzie� wstawi� z powrotem.  Wstawianie
usuni�tego tekstu to "wklejanie".  Mo�esz wkleja� usuni�ty tekst b�d�
to w to samo miejsce, z kt�rego zosta� usuni�ty, b�d� to w inne
miejsca.  Ten sam tekst mo�esz wkleja� kilkukrotnie, w celu uzyskania
wielu kopii.  Poleceniem, kt�re wkleja tekst jest C-y.

Zauwa� r�nic� pomi�dzy "wycinaniem" i "usuwaniem", polegaj�c� na tym,
ze wyci�te rzeczy mog� by� wklejone na nowo, natomiast usuni�te nie.
W og�lno�ci, polecenia, kt�re kasuj� du�o tekstu zachowuj� go, podczas
gdy polecenia, kt�re usuwaj� po prostu jeden znak lub puste linie i
przerwy, nie zachowuj� usuni�tego tekstu.

>> Przesu� kursor na pocz�tek linii, kt�ra nie jest pusta.  Naci�nij
   C-k, by wyci�� tekst z tej linii.

>> Naci�nij C-k jeszcze raz.  Zauwa�, ze wycina to znak nowej linii,
   kt�ry znajduje si� za ta linia.

Zwr�� uwag�, ze pojedyncze C-k wycina zawarto�� linii, a powt�rne C-k
wycina sam� linie tak, �e pozosta�e linie przesuwaj� si� do g�ry.  C-k
traktuje argument liczbowy w specjalny spos�b: wycina ono tyle linii
ORAZ ich zawarto��.  To nie jest samo powtarzanie kilka razy C-k.  C-u
2 C-k wycina dwie linie i ich znaki nowej linii; dwukrotne naci�niecie
C-k nie zrobi�oby tego.

By odzyska� ostatnio wyci�ty tekst i wstawi� go w miejsce kursora,
naci�nij C-y.

>> Twoja kolej.  Naci�nij C-y, by z powrotem wstawi� tekst.

Zwr�� uwag�, ze je�li naci�niesz C-k kilka razy pod rz�d, ca�y wyci�ty
tekst jest zachowywany w jednym kawa�ku tak, �e jedno C-y wklei
wszystkie linie.

>> Naci�nij C-k kilka razy.

By odzyska� ten wyci�ty tekst...

>> ...naci�nij C-y.  Przesu� potem kursor o kilka linii w d� i
   naci�nij C-y jeszcze raz.  Widzisz, ze wstawia to ten sam tekst.

Co zrobi�, je�li chcesz wstawi� tekst, kt�ry wcze�niej wyci��e�, a
potem wycinasz cos innego?  C-y wstawia tekst ostatnio wyci�ty.
Poprzedni fragment nie jest jednak stracony.  Mo�esz wr�ci� do niego,
u�ywaj�c polecenia M-y.  Po tym, jak naci�niesz C-y, by wstawi�
ostatnio wyci�ty tekst, naci�niecie M-y zast�puje wstawiony tekst
poprzednio wyci�tym.  Dalsze naciskanie M-y przywo�uje coraz
wcze�niejsze fragmenty tekstu.  Gdy dojdziesz do tekstu, kt�rego
szuka�e�, nie musisz robi� nic, by zosta� on we w�a�ciwym miejscu.  Po
prostu kontynuuj edycj� tekstu, pozostawiaj�c wklejony tekst tam,
gdzie si� znajduje.

Je�li b�dziesz naciska� M-y wystarczaj�co wiele razy, dojdziesz do
punktu, z kt�rego wystartowa�e� (tekst ostatnio wyci�ty).

>> Wytnij jak�� line, zmie� pozycj� kursora i wytnij inna.  Naci�nij
   potem C-y by wstawi� druga z wyci�tych linii.  Potem naci�nij M-y,
   i linia ta zostanie zast�piona przez ta pierwsza.  Naci�nij M-y
   jeszcze kilka razy, by zobaczy� co si� dzieje.  Powtarzaj to tak
   d�ugo, a� druga z linii pojawi si� z powrotem.  Je�li chcesz,
   mo�esz pod�� M-y dodatnie i ujemne argumenty.


* COFNIJ
--------

Je�li wprowadzisz zmiany do tekstu, a potem dojdziesz do wniosku, �e
to by�a pomy�ka, mo�esz cofn�� te zmiany za pomoc� polecenia "cofnij"
(ang. undo), C-x u.

C-x u cofa zmiany wprowadzone przez jedno polecenie; je�li powt�rzysz
C-x u kilka razy pod rz�d, ka�de powt�rzenie cofa koleje polecenie.

Od tej regu�y s� dwa wyj�tki: polecenia, kt�re nie zmieniaj� tekstu
nie licz� si� jako polecenia, kt�re mo�na cofn�� (zar�wno przesuni�cia
kursora, jak i przewijanie tekstu), oraz znaki wstawiane do tekstu
(np.  litery) ��czone s� w grupy do 20.  (Ma to na celu zredukowanie
liczby naci�ni�� C-x u, kt�re musia�by� wykona�, by cofn�� wstawianie
tekstu.)

>> Wytnij te linie za pomoc� C-k, a potem naci�nij C-x u i linia
   powinna pojawi� si� tu z powrotem.

C-_ jest innym sposobem wywo�ania polecenia "cofnij"; dzia�a to
dok�adnie tak samo jak C-x u, jest jedynie �atwiejsze do naci�ni�cia
kilka razy pod rz�d.  Wada C-_ jest to, ze nie jest to oczywiste w
jaki spos�b nacisn�� te kombinacje na niekt�rych klawiaturach.  To
w�a�nie dlatego C-x u jest tak�e dost�pne.  Na niekt�rych terminalach
mo�esz nacisn�� C-_ poprzez przytrzymanie CTRL i naci�niecie /.

Argument liczbowy podany przed C-_ lub C-x u okre�la liczb� powt�rze�
tego polecenia.


* PLIKI
-------

Aby edytowny przez Ciebie tekst zosta� nma trwa�e zachowany, musisz
umie�ci� go w pliku.  W przeciwnym wypadku zniknie on, gdy Emacs w
kt�rym go edytowa�e� zostanie zamkni�ty.  Zachowywanie Twojego tekstu
w pliku nazywane bywa "odwiedzaniem" lub "znajdywaniem" pliku (ang.
"visiting" lub "finding").

Odwiedzanie pliku oznacza, �e jego zawarto�� zostaje wy�wietlona w
Emacsie.  Bardzo cz�sto sprowadza si� to do edycji samego pliku.
Jednak�e zmiany, kt�re wprowadzasz nie s� trwa�e do momentu, w kt�rym
"zachowasz" plik (ang. save).  Zapobiega to sytuacji, w kt�rej
zostawiasz w systemie plik, kt�ry zosta� tylko w po�owie zmieniony, a
tego nie chcesz zrobi�.  Nawet wtedy, gdy zachowujesz plik, Emacs
zostawia orygina� zachowany pod inna nazwa na wypadek, gdyby� doszed�
do wniosku, �e wprowadzone zmiany by�y b��dne.

Je�li popatrzysz na d� ekranu, zauwa�ysz linie, kt�ra zaczyna i
ko�czy si� my�lnikami i zawiera tekst "XEmacs: TUTORIAL".  W tej
cz�ci ekranu zawsze mo�esz znale�� nazw� pliku, kt�ry w�a�nie
odwiedzasz.  W tej chwili odwiedzasz plik o nazwie "TUTORIAL", kt�ry
jest Twoja w�asn� kopi� podr�cznika Emacsa.  Oboj�tnie jaki plik
odwiedzisz, jego nazwa pojawi si� dok�adnie w tym miejscu.

Polecenia, kt�re s�u�� do odwiedzania i zachowywania plik�w r�ni� si�
od innych polece�, kt�re pozna�e� tym, �e sk�adaj� si� one z dw�ch
znak�w.  Obydwa zaczynaj� si� od znaku Control-x.  Jest mn�stwo
polece�, kt�re zaczynaj� si� od tego w�a�nie znaku; wiele z nich
dotyczy plik�w, bufor�w i z tym zwi�zanych rzeczy.  Polecenia te maj�
d�ugo�� dw�ch, trzech lub czterech znak�w.

Kolejn� nowa rzecz� odno�nie polecenia odwiedzania pliku jest to, �e
musisz mu pod�� nazw� pliku, kt�ry chcesz znale��.  M�wimy o tym, �e
polecenie "czyta argument z terminala" (w tym wypadku argument jest
nazwa pliku).  Po tym, gdy wpiszesz polecenie

	C-x C-f   znajd� plik (ang. find a file)

Emacs poprosi Ci� o wpisanie nazwy pliku.  Nazwa ta pojawia si� w
dolnej linii ekranu.  Lini� t� nazywa si� "minibuforem" (ang.
"minibuffer") wtedy, gdy jest u�ywana do wprowadzania tego typu
danych.  Do edycji nazwy pliku u�ywasz zwyk�ych polece� Emacsa.

Wprowadzanie nazwy pliku (lub jakichkolwiek innych danych w
minibuforze) mo�e zosta� anulowane za pomoc� C-g.

>> Naci�nij C-x C-f, po czym naci�nij C-g.  Na skutek tego zniknie
   minibufor oraz przerwane zostanie polecenie C-x C-f, kt�re tego
   minibufora u�ywa�o.  W rezultacie wi�c nie odwiedzisz �adnego
   pliku.

Gdy sko�czysz wpisywa� nazw� pliku, naci�nij <Return>, po czym
polecenie C-x C-f zabierze si� do roboty i znajdzie plik, kt�ry
wybra�e�.  Minibufor znika z chwil� zako�czenia wykonywania polecenia
C-x C-f.

Po chwili zawarto�� pliku pojawia si� na ekranie i mo�esz j� edytowa�.
Gdy chcesz zachowa� zmiany, tak by je utrwali�, wydaj polecenie

	C-x C-s   zachowaj plik (ang. save).

Kopiuje to tekst z Emacsa do pliku.  Za pierwszym razem gdy to robisz
Emacs zmienia nazw� oryginalnego pliku poprzez dodanie "~" na ko�cu
jego nazwy.

Gdy zachowywanie sko�czy si�, Emacs wypisuje nazw� zapisanego pliku.
Pliki powiniene� zachowywa� stosunkowo cz�sto, tak by nie straci� za
du�o w przypadku za�amania systemu.

>> Naci�nij C-x C-s by zachowa� swoja kopie podr�cznika.  Emacs
   powinien wypisa� "Wrote ...TUTORIAL" na dole ekranu.

UWAGA: W niekt�rych systemach naci�niecie C-x C-s zamrozi ekran i nie
zobaczysz �adnego tekstu z Emacsa.  Oznacza to, �e sk�adowa systemu
operacyjnego, zwana kontrol� przep�ywu (ang. flow control)
przechwyci�a C-s i nie pozwoli�a mu doj�� do Emacsa.  By odzyska�
kontrole nad ekranem, naci�nij C-q.  Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podr�czniku
Emacsa.

Mo�esz odwiedzi� istniej�ce pliki w celu ich edycji lub czytania.
Mo�esz tak�e odwiedzi� plik, kt�ry jeszcze nie istnieje.  W ten
w�a�nie spos�b tworzy si� w Emacsie nowe pliki: odwied� plik, co da Ci
nowe puste miejsce, a potem zacznij wstawia� tekst.  Gdy za��dasz
zachowania pliku, wtedy Emacs naprawd� utworzy plik z tekstem, kt�ry
wpisa�e�.  Od tego momentu mo�esz uwa�a�, �e edytujesz istniej�cy
plik.


* BUFORY
--------

Je�li odwiedzisz inny plik za pomoc� C-x C-f, poprzedni plik pozostaje
w Emacsie.  Mo�esz prze��czy� si� do niego, odwiedzaj�c go jeszcze raz
za pomoc� C-x C-f.  W ten spos�b mo�esz w Emacsie mi�� ca�kiem du�o
plik�w.

>> Utw�rz plik o nazwie "foo" za pomoc� C-x C-f foo <Return>.
   Wpisz w niego jaki� tekst i zachowaj "foo" za pomoc� C-x C-s.
   W ko�cu napisz C-x C-f TUTORIAL <Return>, by wr�ci� do podr�cznika.

Emacs przechowuje tekst ka�dego pliku w obiekcie, zwanym "buforem".
Odwiedzenie pliku tworzy nowy bufor wewn�trz Emacsa.  By zobaczy�
list� bufor�w, kt�re istniej� w Twoim Emacsie, naci�nij

	C-x C-b   lista bufor�w (ang. list buffers).

>> Naci�nij C-x C-b.

Zwr�� uwag�, ze ka�dy bufor ma w�asn� nazw�, mo�e tak�e mie�
skojarzon� z sob� nazw� pliku, kt�ry zawiera.  Pewne bufory nie
odpowiadaj� �adnym plikom.  Na przyk�ad bufor "*Buffer List*" nie
odwiedza �adnego pliku.  Jest to bufor, kt�ry zawiera list� bufor�w
stworzona przez Twoje naci�niecie C-x C-b.  DOWOLNY tekst, kt�ry
ogl�dasz w oknie Emacsa jest zawsze cz�ci� jakiego� bufora.

>> Naci�nij C-x 1 by pozby� si� listy bufor�w.

Je�li dokonujesz zmian tekstu w jakim� pliku, a potem odwiedzisz inny
plik, zawarto�� tego pierwszego NIE jest automatycznie zachowywana.
Zmiany, kt�re wprowadzi�e� pozostaj� w Emacsie, w buforze tego� pliku.
Tworzenie czy edytowanie innego bufora nie ma �adnego wp�ywu na ten
pierwszy.  Jest to bardzo przydatne, ale oznacza tak�e, �e potrzebny
jest Ci wygodny spos�b zachowywania zawarto�ci Twoich bufor�w.
Prze��czanie si� z powrotem do pierwszego bufora zawsze przy
wykonywaniu C-x C-f tylko po to, by nacisn�� tam C-x C-s by�oby
niewygodne.  Dlatego istnieje polecenie:

	C-x s     Zachowaj bufory (ang. save some buffers)

C-x s pyta Ci�, czy chcesz zachowa� ka�dy z bufor�w, w kt�rym
dokona�e� pewnych nie zachowanych jeszcze zmian.

>> Wstaw jak�� lini� tekstu, a potem naci�nij C-x s.
   Powiniene� zosta� zapytany o to, czy chcesz zachowa� bufor
   TUTORIAL.  Odpowiedz na to pytanie twierdz�co naciskaj�c "y".

* U�YWANIE MENU
---------------

Je�li siedzisz przy X-terminalu zauwa�ysz u g�ry okna Emacsa pasek
menu.  Mo�esz u�ywa� menu by dotrze� do najpopularniejszych polece�
Emacsa, takich jak "find file".  Na pocz�tku b�dziesz s�dzi�, ze jest
to �atwiejsze ni� klawiatura, poniewa� nie musisz uczy� si� na pami��
kombinacji klawiszy uruchamiaj�cych jakie� polecenie.  Gdy ju�
zaznajomisz si� z Emacsem, b�dziesz m�g� zacz�� uczy� si� klawiszy ---
elementy menu pokazuj� kombinacje klawiszy, kt�ra wywo�uje dane
polecenie.

Zwr�� uwag�, ze pewne polecenia w menu nie maja jednoznacznych
odpowiednik�w klawiszowych.  Na przyk�ad menu "Buffers" zawiera list�
wszystkich dost�pnych bufor�w.  Mo�esz prze��czy� si� do dowolnego z
nich wybieraj�c jego nazw� z menu Buffers.


* U�YWANIE MYSZY
----------------

Emacs potrafi w pe�ni wykorzystywa� mysz, je�li tylko jest uruchomiony
pod X-Windows.  Mo�esz zmienia� pozycje kursora poprzez naci�niecie
lewego klawisza myszy w po��danym miejscu, mo�esz tak�e zaznacza�
tekst przez przesuniecie myszy z wci�ni�tym lewym klawiszem nad
tekstem, kt�ry chcesz zaznaczy�.  (Innym sposobem jest klikni�cie na
jednym z ko�c�w obszaru, przesuni�cie myszy na drugi koniec i
klikni�cie tam z jednoczesnym wci�ni�ciem klawisza Shift.)

By wyci�� zaznaczony tekst mo�esz nacisn�� C-w lub wybra� Cut z menu
Edit.  Zwr�� uwag� na to, ze *nie* s� to r�wnowa�ne polecenia: C-w
zapami�tuje zaznaczony tekst tylko wewn�trz Emacsa (podobnie jak
om�wione powy�ej C-k), natomiast Cut robi to oraz umieszcza ten tekst
w schowku systemu X, sk�d mo�e on zosta� pobrany przez inne programy.

By wklei� tekst ze schowka systemu X-Windows u�yj polecenia Paste z
menu Edit.

�rodkowy klawisz myszy jest cz�sto u�ywany do wybierania element�w,
kt�re s� wy�wietlone na ekranie.  Na przyk�ad, je�li uruchomisz Info
(system dokumentacji Emacsa) naciskaj�c C-h i, lub wybieraj�c ten
element z menu Help, przej�cie pod�wietlonym po��czeniem (ang. link)
odbywa si� poprzez naci�niecie �rodkowego klawisza myszy.  Podobnie,
je�li wpisujesz nazw� pliku (np. podczas wykonywania "Find File") i
naci�niesz TAB, by zobaczy� wszystkie mo�liwe doko�czenia nazwy,
mo�esz wybra� jedno z nich z wy�wietlonej listy, w�a�nie naciskaj�c
�rodkowy klawisz myszy.

Prawy klawisz myszy pokazuje lokalne menu.  Zawarto�� tego menu zale�y
od trybu pracy Emacsa, w kt�rym aktualnie jeste�, i zawiera kilka
cz�sto u�ywanych polece�, tak by by�y one �atwiejsze w dost�pie.

>> Naci�nij prawy klawisz myszy

Prawy klawisz myszy musi by� trzymany, by menu nie znik�o
automatycznie.


* ROZSZERZANIE ZESTAWU POLECEN
------------------------------

Polece� Emacsa jest du�o du�o wi�cej, ni� mo�na by skojarzy�
kombinacjami zwyk�ych klawiszy oraz META czy CTRL.  Emacs radzi sobie
z tym za pomoc� polecenia X (ang. eXtend).  Istniej� jego dwa rodzaje:

	C-x	Rozszerzenie o znak.  Nast�puje po nim jeden znak.
	M-x	Rozszerzenie o nazwane polecenie.  Nast�puje po nim
                pe�na d�uga nazwa polecenia.  

Polecenia te w og�lno�ci s� u�yteczne, ale s� u�ywane nie tak cz�sto
jak polecenia, kt�rych ju� si� nauczy�e�.  Mia�e� ju� okazje pozna�
dwa z nich: C-x C-f s�u��ce do odwiedzania plik�w oraz C-x C-s do ich
zachowywania.  Innym przyk�adem mo�e by� polecenie, kt�re ko�czy sesje
Emacsa C-x C-c.  (Nie martw si�, ze mo�esz w ten spos�b straci�
zmiany, kt�re dokona�e�; C-x C-c oferuje Ci mo�liwo�� zachowania
ka�dego ze zmodyfikowanych plik�w przed zamkni�ciem Emacsa.)

C-z jest poleceniem, kt�re wychodzi z Emacsa *na chwile*, tak by� m�g�
wr�ci� do tej samej sesji Emacsa po jakim� czasie.

W systemach, w kt�rych jest to mo�liwe, C-z zawiesza proces Emacsa;
powoduje to powr�t do pow�oki (ang.  shell), ale nie niszczy Emacsa.
W najpopularniejszych pow�okach mo�esz wr�ci� do Emacsa za pomoc�
polecenia `fg' lub `%emacs'.

W systemach, w kt�rych zawieszanie proces�w nie dzia�a, C-z tworzy
proces podpow�oki (ang. "subshell"), kt�ry dzia�a pod Emacsem i daje
Ci szans� uruchamiania innych program�w i powrotu do Emacsa po ich
sko�czeniu; w tych systemach C-z nie wychodzi naprawd� z Emacsa.  W
tych wypadkach normalnym poleceniem powrotu do Emacsa jest wyj�cie z
podpow�oki za pomoc� "exit".

Polecenia C-x C-c powiniene� u�ywa�, gdy masz si� wylogowa�.  Zalecane
jest tak�e wychodzenie z Emacsa wystartowanego przez np. programy
obs�uguj�ce poczt� elektroniczna lub innego rodzaju narz�dzia,
poniewa� mog� one nie wiedzie� jak sobie poradzi� z zawieszeniem
Emacsa.  Jednak�e w zwyk�ych okoliczno�ciach, je�li nie musisz
wylogowywa� si� z systemu, lepiej jest zawiesi� Emacsa za pomoc� C-z
ni� z niego wyj��.

Istnieje wiele polece� zaczynaj�cych si� od C-x.  Oto lista tych,
kt�rych si� ju� nauczy�e�:

	C-x C-f           odwied� plik
	C-x C-s           zachowaj plik
	C-x C-b           wy�wietl list� bufor�w
	C-x C-c           wyjd� z Emacsa
	C-x u             cofnij

Polece� podawanych za pomoc� nazwy u�ywa si� jeszcze rzadziej lub
u�ywa si� tylko w pewnych trybach.  Przyk�adem mo�e by� polecenie
replace-string, kt�re globalnie zast�puje jeden �a�cuch innym.  Gdy
naciskasz M-x, Emacs czeka na ci�g dalszy, wy�wietlaj�c "M-x" na dole
ekranu.  Powiniene� po tym wpisa� nazw� polecenia, w tym wypadku
"replace-string".  Napisz tylko "repl s<TAB>", a Emacs doko�czy nazw�.
Zako�cz wprowadzanie nazwy przez naci�niecie klawisza <Return>.

Polecenie replace-string wymaga dw�ch argument�w: �a�cucha, kt�ry ma
zosta� zast�powany i �a�cucha, kt�ry ma by� wstawiony w miejsce tego�.
Obydwa �a�cuchy musza by� zako�czone przyci�ni�ciem <Return>.

>> Przesu� kursor do czystej linii, dwie linie poni�ej tej.
   Naci�nij M-x repl s<Return>zmieni<Return>zmodyfikuje<Return>.

Zwr�� uwag� na to, jak ta linia si� zmieni: zast�pi�e� s�owem
"zmodyfikuje" ka�de wyst�pienie s�owa z-m-i-e-n-i poni�ej pocz�tkowej
pozycji kursora.


* AUTOMATYCZNE ZACHOWYWANIE
---------------------------

Gdy wprowadzisz zmiany do pliku i ich nie zachowasz, mog� one zosta�
stracone, je�li Tw�j komputer przestanie dzia�a�.  By uchroni� Ci�
przed tym, Emacs okresowo zapisuje specjalny plik z wprowadzonymi
zmianami.  Plik ten ma znak # na pocz�tku i na ko�cu swojej nazwy.  Na
przyk�ad, za��my, ze Tw�j plik nazywa si� "hello.c".  Odpowiadaj�cy
mu plik automatycznie zachowywany b�dzie nosi� nazw� "#hello.c#".  Gdy
zachowujesz plik w zwyk�y spos�b, Emacs kasuje plik automatycznie
zachowany.

Je�li Tw�j komputer przestanie dzia�a�, mo�esz odzyska� Twoje dane z
pliku automatycznie zachowanego przez zwykle odwiedzenie pliku (tego,
kt�ry edytowa�e�, a nie pliku automatycznie zachowanego) i napisanie
M-x recover file<return>.  Gdy Emacs zapyta o potwierdzenie, napisz
yes<return> by odzyska� dane, kt�re zosta�y automatycznie zachowane.


* OBSZAR ECHA
-------------

Je�li polecenia dla Emacsa wpisujesz dostatecznie wolno, zostan� one
pokazywane w specjalnym obszarze na dole ekranu, zwanym obszarem echa
(ang. echo area).  Obszar echa zawiera ostatnia dolna linie ekranu.


* LINIA STANU
-------------

Linia, kt�ra znajduje si� bezpo�rednio nad obszarem echa, zwana jest
"lini� trybu" (ang. modeline).  Pokazuje ona tekst podobny do
nast�puj�cego:

--**-XEmacs: TUTORIAL         (Fundamental)--L670--58%----------------

Linia ta podaje u�yteczne informacje o stanie Emacsa i tekstu, kt�ry
edytujesz.  Wiesz ju�, jakie jest znaczenie nazwy pliku: jest to plik,
kt�ry odwiedzi�e�.  --NN%-- opisuje Twoja bie��c� pozycje wewn�trz
tekstu; oznacza to, �e NN procent tekstu znajduje si� ponad g�rnym
brzegiem ekranu.  Je�li pocz�tek pliku znajduje si� na pocz�tku
ekranu, zamiast liczby --00%-- zobaczysz w tym miejscu --Top--.
Podobnie dla ko�ca tekstu pojawi si� tam napis --Bot-- (od
ang. bottom).  Je�li wy�wietlasz tekst na tyle kr�tki, ze mie�ci si� w
ca�o�ci na ekranie, linia stanu poka�e --All--.

Gwiazdki blisko pocz�tku linii trybu oznaczaj�, ze wprowadzi�e� do
tekstu jakie� zmiany.  Bezpo�rednio po odwiedzeniu lub po zachowaniu
pliku nie b�dzie w tym miejscu �adnych gwiazdek, a tylko my�lniki.

Wewn�trz nawias�w znajdziesz informacje na temat trybu edycji, w
kt�rym w�a�nie jest Emacs.  Domy�lnym trybem edycji jest tryb
podstawowy (ang. fundamental), kt�ry jest trybem (w�a�nie w tej chwili
u�ywanym--) u�ywanym w�a�nie w tej chwili.  Jest to przyk�ad "trybu
g��wnego" (ang. major mode).

Emacs mo�e dzia�a� w wielu trybach g��wnych.  Pewne z nich zosta�y
zaprojektowane do edycji rozmaitych j�zyk�w i/lub rodzaj�w tekstu,
takie jak tryb Lispu, tryb tekstowy, itd.  W danej chwili mo�e by�
aktywny tylko jeden g��wny tryb pracy, i to jego nazwa jest
wy�wietlana w linii trybu w miejscu, w kt�rym teraz jest
"Fundamental".

Ka�dy z g��wnych tryb�w edycyjnych mo�e zmieni� zachowanie niekt�rych
polece�.  Na przyk�ad, w Emacsie istniej� polecenia s�u��ce do
tworzenia komentarzy w programach.  Ka�dy j�zyk programowania na sw�j
spos�b okre�la, jak powinien wygl�da� komentarz, tak wiec ka�dy z
g��wnych tryb�w edycyjnych musi wstawia� komentarze w specyficzny
spos�b.  Ka�dy tryb edycyjny jest nazw� polecenia, kt�re mo�esz
wykona�, by prze��czy� si� w ten tryb lub wy��czy� ten tryb.
Przyk�adem mo�e by� M-x fundamental-mode, kt�re jest poleceniem
prze��czaj�cym tryb podstawowy.

Je�li zamierzasz edytowa� tekst w j�zyku angielskim, taki jak na
przyk�ad oryginalna wersja tego podr�cznika, prawdopodobnie powiniene�
u�ywa� trybu tekstowego (ang. text mode).

>> Napisz M-x text-mode<Return>.

Nie musisz si� martwi�, �adne z polece�, kt�re do tej pory pozna�e�,
nie zmienia Emacsa w powa�ny spos�b.  Mo�esz jednak zauwa�y�, ze teraz
M-f i M-b traktuj� apostrofy jako cz�ci s��w.  Poprzednio, w trybie
podstawowym, polecenia te traktowa�y apostrofy jako separatory s��w.

G��wne tryby edycji wprowadzaj� zwykle subtelne zmiany, takie jak
opisana powy�ej: wi�kszo�� polece� robi dalej "to samo", robi to
jednak w spos�b troszeczk� inny.

By zobaczy� dokumentacj� na temat bie��cego g��wnego trybu edycji,
naci�nij C-h m.

>> Naci�nij C-u C-v raz lub wi�cej razy tak, by ta linia znalaz�a si�
   blisko g�ry ekranu.

>> Naci�nij C-h m, by zobaczy� jak tryb tekstowy r�ni si� od trybu
   podstawowego. 

>> Naci�nij q, by usun�� dokumentacje z ekranu.

G��wne tryby edycji nazywaj� si� "g��wnymi", poniewa� s� tak�e
podrz�dne tryby edycji (ang. minor modes).  Podrz�dne tryby edycji nie
s� alternatyw� dla g��wnych tryb�w edycji, a jedynie ich niewielk�
modyfikacj�.  Ka�dy podrz�dny tryb edycji mo�e zosta� w��czony lub
wy��czony niezale�nie od pozosta�ych podrz�dnych tryb�w edycji oraz
niezale�nie od g��wnego trybu edycji.  Mo�esz wiec u�ywa� jednego,
kombinacji dowolnych, lub nie u�ywa� �adnego trybu podrz�dnego.

Jednym z podrz�dnych tryb�w edycji, kt�ry jest bardzo u�yteczny
szczeg�lnie do edycji tekstu angielskiego, jest tryb automatycznego
wype�niania (ang. auto fill mode).  Je�li ten tryb jest w��czony,
Emacs lamie linie pomi�dzy s�owami automatycznie, gdy wstawiasz tekst
i linia robi si� za szeroka.

Tryb automatycznego wstawiania w��cza si� na przyk�ad poprzez
wywo�anie polecenia M-x auto-fill-mode<Return>.  Je�li ten tryb jest
w��czony to samo polecenie wy��cza go, i vice versa.  M�wimy, ze
polecenie to "prze��cza ten tryb".

>> Napisz M-x auto-fill-mode<Return>.  Wstawiaj potem lini� pe�n�
   "asdf " tak d�ugo, a� zobaczysz, �e si� podzieli na dwie linie.
   Musisz wstawi� spacje pomi�dzy znaki, poniewa� tryb automatycznego
   wype�niania �amie linie tylko tam, gdzie s� spacje.

Margines jest zazwyczaj ustawiony na 70 znak�w, ale mo�esz go zmieni�
za pomoc� polecenia C-x f.  Powiniene� poda� mu argument liczbowy
m�wi�cy, w kt�rej kolumnie ma zosta� ustawiony margines.

>> Wywo�aj C-x f z argumentem r�wnym 20. (C-u 2 0 C-x f).
   Napisz potem jaki� tekst i zauwa�, ze Emacs wype�nia linie do
   d�ugo�ci co najwy�ej 20 znak�w.  Ustaw margines z powrotem na
   70 znak�w, wywo�uj�c jeszcze raz C-x f.

Je�li dokonujesz zmian wewn�trz akapitu, tryb 
automatycznego wype�niania nie wyr�wna marginesu
sam z siebie.  By wywo�a� polecenie
wyr�wnania marginesu, naci�nij M-q (Meta-q), 
podczas gdy kursor znajduje si� wewn�trz akapitu.

>> Przesu� kursor do poprzedniego akapitu i naci�nij M-q.


* SZUKANIE
----------

Emacs potrafi szuka� �a�cuch�w (zwartych ci�g�w znak�w lub s��w)
zar�wno wstecz jaki i do przodu.  Szukanie �a�cucha jest poleceniem,
kt�re przesuwa kursor; przesuwa ono kursor do nast�pnego miejsca, w
kt�rym dany �a�cuch wyst�puje.

Polecenie Emacsa "search" r�ni si� od podobnych polece� innych
edytor�w w tym sensie, ze jest ono przyrostowe.  Znaczy to, ze
szukanie odbywa si� w trakcie, gdy Ty wpisujesz kolejne znaki
�a�cucha, kt�ry ma zosta� odnaleziony.

Poleceniami zapocz�tkowuj�cymi szukanie s�: C-s dla szukania w prz�d
oraz C-r dla szukania wstecz.  POCZEKAJ PROSZ�!  Nie pr�buj ich w tej
chwili.

Gdy naci�niesz C-s zauwa�ysz, ze tekst "I-search" pojawi si� w
obszarze echa.  Informuje Ci� to, �e Emacs znajduje si� w trybie
"incremental search", czekaj�c by� napisa� tekst, kt�ry ma on znale��.
Naci�niecie <Return> ko�czy proces szukania.

>> Teraz naci�nij C-s, by rozpocz�� szukanie.  POWOLI, litera po
   literze, napisz s�owo "kursor", zatrzymuj�c si� po ka�dym znaku i
   obserwuj�c, gdzie zatrzymuje si� kursor.  Gdy naci�niesz drugie
   "r", b�dzie mo�na powiedzie�, �e szuka�e� s�owa "kursor"
   jednokrotnie.  Naci�nij C-s jeszcze raz, by znale�� nast�pne
   wyst�pienie s�owa "kursor".  Naci�nij teraz <Delete> cztery
   razy i zobacz, co si� dzieje z kursorem.  Naci�nij <RET>, by sko�czy�
   szukanie.

Widzia�e�, co si� dzia�o? Emacs podczas szukania przyrostowego pr�buje
przej�� do miejsca wyst�pienia �a�cucha, kt�ry do tej pory wpisa�e�,
pod�wietlaj�c go dla Twojej wygody.  By znale�� nast�pne wyst�pienie
s�owa "kursor", po prostu naci�nij C-s jeszcze raz.  Je�li takiego nie
ma, Emacs zapiszczy i powie Ci, ze szukanie "sko�czy�o si� pora�k�".
Naci�niecie C-g tak�e przerywa proces szukania.

UWAGA: W niekt�rych systemach naci�niecie C-s zamrozi ekran i nie
zobaczysz �adnego tekstu z Emacsa.  Oznacza to, �e sk�adowa systemu
operacyjnego, zwana kontrol� przep�ywu (ang. "flow control")
przechwyci�a C-s i nie pozwoli�a mu dojs� do Emacsa.  By odzyska�
kontrole nad ekranem, nacisnij C-q.  Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podr�czniku
Emacsa.

Jesli podczas szukania przyrostowego naci�niesz <Delete> zauwa�ysz, ze
ostatni znak, kt�ry wcisn��es znika i kursor wraca do poprzedniego
miejsca.  Na przyk�ad, za��my, ze nacisn��es "k" i znalaz�es pierwsze
wyst�pienie tej litery.  Jesli teraz naci�niesz "u", kursor przesunie
si� do pierwszego wyst�pienia "ku".  Teraz nacisnij <Delete>.  Skasuje
to "u" z �a�cucha, kt�rego poszukujesz, a kursor wr�ci do pierwszego
wyst�pienia "k".

Je�li podczas szukania nacisniesz jaki� klawisz razem z META lub CTRL
(z nielicznymi wyj�tkami --- znakami, kt�re maj� specjalne znaczenie
podczas szukania, takimi jak C-s i C-r) szukanie zostanie przerwane.

C-s rozpoczyna proces szukania, kt�ry poszukuje �a�cucha, kt�ry
znajduje si� ZA bie��c� pozycja kursora.  Je�li chcesz szuka� czego�
wcze�niej w tek�cie, naci�nij C-r.  Wszystko, co powiedzieli�my o C-s
stosuje si� do C-r, oczywi�cie ze zmian� kierunku szukania na wstecz.


* WIELE OKIEN
-------------

Jedn� z przyjemnych cech Emacsa jest mo�liwo�� wy�wietlania wi�cej ni�
jednego okna na raz.

>> Przesu� kursor do tej linii i naci�nij C-u 0 C-l.

>> Naci�nij teraz C-x 2, co podzieli ekran na dwa okna.  Obydwa okna
   wy�wietlaj� ten podr�cznik.  Kursor pozostaje w g�rnym oknie.

>> Naci�nij C-M-v by przewin�� dolne okno.  (Je�li nie masz
   prawdziwego klawisza Meta, naci�nij ESC C-v.)

>> Naci�nij C-x o ("o" jak angielskie "other") by przesun�� kursor do
   dolnego okna.  U�yj C-v i M-v w dolnym oknie by przewin�� jego
   zawarto��.  Polecenia, kt�re masz wykona� czytaj w g�rnym oknie.

>> Naci�nij C-x o jeszcze raz tak, by kursor wr�ci� do g�rnego okna.
   Kursor w g�rnym oknie nie zmieni� po�o�enia.

Ka�de okno pami�ta po�o�enie swojego w�asnego kursora, lecz tylko
jedno okno w danej chwili wy�wietla kursor.  Wszystkie polecenia
edycyjne stosuj� si� do okna, w kt�rym jest kursor.  To okno nazywane
jest "wybranym oknem".

Polecenie C-M-v jest bardzo u�yteczne gdy edytujesz tekst w jednym
oknie, a drugiego u�ywasz tylko jako punkt odniesienia.  Dzi�ki temu
kursor mo�e zawsze znajdowa� si� w oknie, zawarto�� kt�rego edytujesz,
a Ty mo�esz przesuwa� drugie okno za pomoc� C-M-v.

C-M-v to przyk�ad znaku, kt�ry uzyskuje si� za pomoc� CONTROL-META.
Je�li masz prawdziwy klawisz META, C-M-v mo�esz uzyska� przytrzymuj�c
jednocze�nie CTRL oraz META i naciskaj�c v.  Nie jest wa�ne, co
zosta�o naci�niete wcze�niej, CTRL czy META, poniewa� obydwa te
klawisze dzia�aj� jako modyfikatory znaczenia klawiszy, kt�re
naciskasz.

Je�li nie masz klawisza META i u�ywasz w jego zast�pstwie ESC,
kolejno�� naciskania klawiszy jest znacz�ca: musisz najpierw nacisn��
i pu�ci� ESC, po czym nacisn�� CTRL-v; CTRL-ESC v nie b�dzie dzia�a�.
Dzieje si� tak dlatego, ze ESC jest znakiem, a nie modyfikatorem.

>> Naci�nij C-x 1 (w g�rnym oknie), by pozby� si� dolnego okna.

(Je�li nacisn��by� C-x 1 w dolnym oknie, to znik�oby g�rne.  Mo�esz
sobie t�umaczy� to polecenie jako "zatrzymaj tylko jedno okno --- to w
kt�rym w�a�nie jestem".)

Nie musisz wy�wietla� tego samego bufora w obydwu oknach.  Je�li
u�yjesz C-x C-f by wy�wietli� plik w jednym z okien, zawarto��
drugiego nie zmieni si�.  W ka�dym oknie mo�esz wy�wietla� r�ne pliki
niezale�nie.

Oto inny spos�b u�ywania dw�ch okien do wy�wietlania dw�ch r�nych
rzeczy:

>> Naci�nij C-x 4 C-f i nazw� jednego z Twoich plik�w.  Zako�cz
   wprowadzanie klawiszem <Return>.  Podany plik pojawi si� w dolnym
   oknie razem z kursorem, kt�ry tam przeskakuje.

>> Naci�nij C-x o, by wr�ci� do g�rnego okna, oraz C-x 1 by
   usun�� dolne okno.


* REKURSYWNE POZIOMY EDYCJI
---------------------------

Czasami mo�esz znale�� si� w czym�, co nazywa si� "rekursywnym
poziomem edycji".  Mo�esz to rozpozna� po nawiasach kwadratowych w
linii trybu, obejmuj�cych nawiasy okr�g�e zawieraj�ce nazw� g��wnego
trybu edycji.  Na przyk�ad, m�g�by� widzie� [(Fundamental)] zamiast
(Fundamental).

By wyj�� z rekursywnego poziomu edycji naci�nij ESC ESC ESC.  Jest to
og�lnego przeznaczenia "wychodzimy".  Mo�esz go u�y� tak�e by pozby�
si� nadmiaru okien lub wyj�� z minibufora.

>> Naci�nij M-x by wej�� do minibufora; naci�nij potem ESC ESC ESC, by
   z niego wyj��.

Nie mo�esz u�y� C-g, by wyj�� z rekursywnego poziomu edycji.  Dzieje
si� tak dlatego, ze C-g jest u�ywane do anulowania polece� i
argument�w WEWN�TRZ rekursywnego poziomu edycji.


SZUKANIE DODATKOWEJ POMOCY
--------------------------

W tym podr�czniku spr�bowali�my dostarczy� tylko tyle informacji, ile
jest niezb�dne, by� m�g� zacz�� u�ywa� Emacsa.  Emacs jest istn�
kopalni� najr�niejszych rzeczy, kt�rych nie spos�b tutaj opisa�.
Zapewne b�dziesz chcia� dowiedzie� si� wi�cej o Emacsie, poniewa�
posiada on wiele po��danych cech, o kt�rych na razie nic nie wiesz.
Jest w nim zaszyte mn�stwo wewn�trznej dokumentacji, kt�ra mo�e by�
osi�gni�ta za pomoc� Control-h, kt�re okre�lamy mianem "znaku pomocy"
z powodu spe�nianej przez niego roli.

By uzyska� pomoc, naci�nij C-h a potem znak, kt�ry okre�la jakiego
typu pomocy oczekujesz.  Je�li poczujesz si� NAPRAWD� zagubiony,
napisz C-h ? i Emacs spr�buje powiedzie� Ci, jakiego typu pomocy mo�e
Ci dostarczy�.  Je�li naci�niesz C-h a potem zadecydujesz, �e pomoc
nie jest Ci jednak potrzebna, po prostu wci�nij C-g by anulowa� C-h.

Najprostsz� pomoc mo�esz uzyska� naciskaj�c C-h c.  Naci�nij C-h a
potem c, po czym kombinacje klawiszy polecenia, i Emacs wy�wietli
bardzo kr�tki opis polecenia.

>> Naci�nij C-h c Control-p.
   Powinno to przywo�a� komunikat, o tre�ci podobnej do

	C-p runs the command previous-line

W ten spos�b mo�esz uzyska� "nazw� funkcji".  Przydaje si� to podczas
pisania kodu w Lispie, kt�ry rozszerza Emacsa; wystarcza to tak�e do
przypomnienia Ci, co dane polecenie robi, je�li widzia�e� je ju�
wcze�niej, ale nie zapami�ta�e� go.

Polecenia wywo�ywane za pomoc� wieloznakowej kombinacji klawiszy, na
przyk�ad C-x C-s oraz (je�li nie masz klawisza META lub EDIT) <ESC>v,
s� tak�e dopuszczalne po C-h c.

By uzyska� wi�cej informacji na temat polecenia, naci�nij C-h k
zamiast C-h c.

>> Naci�nij C-h k Control-p.

To polecenie wy�wietla dokumentacj� na temat danej funkcji oraz jej
nazw� w oknie Emacsa.  Gdy sko�czysz �ledzi� wynik tego polecenia
naci�nij C-x 1, by pozby� si� tekstu pomocy.  Nie musisz tego robi� od
razu.  Mo�esz wykona� pewne operacje w oparciu o tekst pomocy zanim
naci�niesz C-x 1.

Oto kilka innych u�ytecznych wariant�w C-h:

   C-h f	Opisz funkcje o podanej nazwie.

>> Napisz C-h f previous-line<Return>.  Wypisze to na ekranie ca��
   informacje, jak� Emacs ma na temat funkcji, kt�ra implementuje
   polecenie C-p.

   C-h a	Apropos.   Wpisz s�owo kluczowe, a Emacs wypisze list�
                wszystkich polece�, kt�rych nazwa zawiera to s�owo.
                Polecenia te mog� zosta� wywo�ane za pomoc� Meta-x.
                Dla niekt�rych polece� Apropos wypisze jedno- lub
                dwuznakowe sekwencje, kt�re wywo�uj� dane polecenie.

>> Napisz C-h a file<Return>.  Zobaczysz list� wszystkich polece�,
   dost�pnych za pomoc� M-x, kt�re maja s�owo "file" w swojej nazwie.
   Zauwa�ysz tam tak�e polecenia takie, jak C-x C-f oraz C-x C-w,
   umieszczone obok nazw polece� "find-file" i "write-file".


PODSUMOWANIE
------------

Pami�taj, �e by wyj�� z Emacsa na sta�e, u�ywaj C-x C-c.  By wyj�� do
pow�oki na chwil� tak, by� m�g� wr�ci�, u�yj C-z. (To nie dzia�a pod
X-Windows, poniewa� tam nie ma prawdziwego konceptu przej�cia na
chwile do pow�oki.  Zamiast tego C-z ikonizuje okno Emacsa.)

Ten podr�cznik by� pisany tak, by wszyscy nowi u�ytkownicy mogli go
zrozumie�.  Je�li co� pozostawi� niejasnym, nie sied� cicho i nie
obwiniaj siebie, tylko daj nam zna�!


KOPIOWANIE
----------

Niniejszy podr�cznik jest potomkiem w d�ugiej linii podr�cznik�w
Emacsa, kt�ra rozpoczyna si� od tego, kt�ry zosta� napisany przez
Stuarta Cracrafta dla oryginalnego Emacsa.  Zosta� on zmodyfikowany we
wrze�niu 1994 przez Bena Winga, kt�ry zaktualizowa� go, je�li chodzi o
X-Windows.

T�umaczenia na j�zyk polski dokona� Remek Trzaska z pomoc� Ryszarda
Kubiaka.  Jesli polskie znaki nie byly poprawnie wyswietlane w tym
buforze, oznacza to, ze nie masz zainstalowanych polskich fontow. 
Pomoc w tym zakresie mozesz znalezc pod adresem: 
               <URL:http://www.agh.edu.pl/ogonki>

Ta wersja podr�cznika, podobnie jak GNU Emacs, jest zastrze�ona, a
pozwolenie na kopiowanie udzielone jest pod nast�puj�cymi warunkami:

Copyright (c) 1985, 1994 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim
   copies of this document as received, in any medium, provided that
   the copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

Warunki kopiowania samego Emacsa s� w pewnym stopniu inne, aczkolwiek
zachowuj� te sama idee.  Prosz�, przeczytaj plik COPYING, po czym
rozdaj swoim znajomym kopie Emacsa.  Pom� zwalczy� przeszkody w
rozpowszechnianiu oprogramowania przez tworzenie i dzielenie si�
oprogramowaniem.
