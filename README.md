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
W korzeniu projektu znajduje się skrypt wykonywalny `latc`.
Ponieważ miałam problemy z wykopiowaniem plików binarnych z katalogów, utworzonych przez stacka, jest to skrypt bashowy, który wywołuje komendę `stack exec latc <given_file_path>`, uruchamiający binarkę kompilatora.
Po uprzednim zbudowaniu projektu polecenim `make` zgodnie z treścią zadania program uruchamia się poleceniem `./latc <file>`.
W katalogu głównym znajdują się również dwa skrypty uruchamiane: `./run_good.sh` oraz `./run_bad.sh`, uruchamiające wszytskie podstawowe testy frontendu, znajdujące się w odpowiednio w katalogach `tests/good` oraz `tests/bad`.

## Parser
Z powodu różnic w wersjach i wygenerowanych kodach załączam wygenerowane już przez bnfc pliki parsera.
Wersje użytych w tym celu narzędzi
* bnfc wersja 2.8.1
* alex wersja 3.2.3
* happy wersja 1.19.8

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
    `Main.hs` - plik główny kompilatora
lib/ zawiera biblioteki
    `runtime.c` - z funkcjami bibliotecznymi w c
tests/ zawiera dwa katalogi z testami good i bad

`latc` - skrypt uruchamiający kompilator

`run_good.sh` - skrypt uruchamijaący kompilator na testach z katalogu good
`run_bad.sh` - skrypt uruchamiający kompilator na testach z katalogu bad

`Latte.Compiler.cabal` - plik konfiguracyjny cabala
`stack.yaml` - plik konfiguracyjny stacka
`Makefile`
`Readme.md`


Aleksandra Falkiewicz 394182