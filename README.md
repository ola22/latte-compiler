# latte-compiler
Drugi projekt zaliczneiowy z MRJP - kompilator języka Latte.


## Budowanie
Do budowania projektu użyłam stacka, stworzoneho na podstawie pliku cabala.
Polecenie `make` buduje projekt (wywołuje komendę `stack build`).
Polecenie `make clean` usuwa poleceniem `stack clean --full`
Do budowania projektu i kompilacji wykorzystuję:
* cabal wersja 3.2.0.0
* ghc wersja 8.8.3
* stack wersja 2.5.1


## Uruchamianie programów
W korzeniu projektu znajduje się skrypt wykonywalny `latc_x86_64`.
Ponieważ miałam problemy z wykopiowaniem plików binarnych z katalogów, utworzonych przez stacka, jest to skrypt bashowy, który wywołuje komendę `stack exec latc_x86_64 <given_file_path>`, uruchamiający binarkę kompilatora.
Po uprzednim zbudowaniu projektu polecenim `make` zgodnie z treścią zadania program uruchamia się poleceniem `./latc_x86_64 <file>`.
W katalogu głównym znajdują się również dwa skrypty uruchamiane: `./run_good.sh` oraz `./run_bad.sh`, uruchamiające wszytskie podstawowe testy, znajdujące się w odpowiednio w katalogach `tests/good` oraz `tests/bad`. W przypadku testów z katalogu good porównywane są również outputy.


## Parser
Z powodu różnic w wersjach i wygenerowanych kodach załączam wygenerowane już przez bnfc pliki parsera.
Wersje użytych w tym celu narzędzi
* bnfc wersja 2.8.1
* alex wersja 3.2.3
* happy wersja 1.19.8


# Kompilator
* kod generowany do asemblera na architekturę x86_64, asemblowany przy uzyciu nasm
* Calling convention - komplator używa własnego calling convention, wszystkie argumenty funkcji znajdują się na stosie.
                        pod adresami kolejno rsp + 16, rsp + 24, .. , a po nich znajduje się opcjonalnie wartość zwracana przez funkcję
* Z powodu innego niż w C calling convention w katalogu `lib` znajduje się dodatkowa biblioteka runtime_helper.asm, która przed wywołaniem
  funkcji z C umieszcza argumenty w odpowiednich rejestrach (zgodnie ze standardem C)
* Int 64bitowy


## Użyte zewnętrzne biblioteki (haskellowe)
array, containers, process, mtl, filepath


# Struktura katalogów i plików projektu

src/ zwiera pliki źródłowe projektu
    Latte/ katalog gramatyki i parsera
        Parsing/  zawiera pliki automatycznie wygenerowane przez parser `pliki parsera`
        `Makefile`
        `Latte.cf` - gramatyka języka Latte
    Common/
        `Common.hs` - plik z powszechnie używanymi funkcjami pomocniczymi
        `ErrorPositions.hs` - zawiera funkcje służace do wydobywania pozycji elementów AST w pliku
    `Compiler.hs` - plik z kodem tworzącym kod asemblera
    `SematicChecker.hs` - plik frontendu kompilatora
    `IRCreator` - plik tworzący reprezentację IR
    `Main.hs` - plik główny kompilatora
lib/ zawiera biblioteki
    `runtime.c` - z funkcjami bibliotecznymi w c
    `runtime_helper.asm` - plik asemblerowy, wywołujący funkcje biblioteczne z C
                           potrzebny z powodu innej niż w C calling convention
    `Makefile` - makefile plików bibliotecznych
    `runtime.o` - objectfile pliku runtime.c
    `runtime_helper.o` - objectfile pliku runtime_helper.asm
tests/ zawiera dwa katalogi z testami good i bad

`latc_x86_64` - skrypt uruchamiający kompilator

`run_good.sh` - skrypt uruchamijaący kompilator na testach z katalogu good i sprawdza outputy
`run_bad.sh` - skrypt uruchamiający kompilator na testach z katalogu bad

`Latte.Compiler.cabal` - plik konfiguracyjny cabala
`stack.yaml` - plik konfiguracyjny stacka
`Makefile`
`Readme.md`


Aleksandra Falkiewicz 394182

