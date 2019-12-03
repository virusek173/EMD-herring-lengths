Herringmania
================

## Wstępna analiza danych

##### Wczytanie danych oraz określenie rozmiaru:

``` r
fileName = "data/sledzie.csv"
herrings = read.csv(fileName, header = TRUE, sep = ",", dec = ".")
dimentions <- dim(herrings)
dimentions
```

    ## [1] 52582    16

Mamy do czynienia z danymi mającymi 16 zmiennych oraz 52582 przypadków
do
    analizy.

##### Wypisanie nazw kolumn oraz pierwszego wiersza:

``` r
head(herrings, n=1)
```

    ##   X length   cfin1   cfin2   chel1 chel2   lcop1    lcop2  fbar   recr
    ## 1 0     23 0.02778 0.27785 2.46875     ? 2.54787 26.35881 0.356 482831
    ##        cumf   totaln           sst      sal xmonth nao
    ## 1 0.3059879 267380.8 14.3069330186 35.51234      7 2.8

Widzimy, że z powodu braku nazwy pierwszej kolumny, została ona
zastąpiona znakiem X. Akceptujemy to uzupełnienie pamiętając, aby w
przyszłości odwoływać się właśnie do tego znaku.

##### Zamiana znaku “?” reprezentującego pustą wartość na <NA>:

``` r
#herrings <- head(herrings, 6)
herrings[herrings == "?"] <- NA
```

##### Sprawdzenie ile jest pustych wartości w każdej kolumnie:

``` r
countNa <- sapply(herrings, function(x) sum(is.na(x)))
countNa
```

    ##      X length  cfin1  cfin2  chel1  chel2  lcop1  lcop2   fbar   recr   cumf 
    ##      0      0   1581   1536   1555   1556   1653   1591      0      0      0 
    ## totaln    sst    sal xmonth    nao 
    ##      0   1584      0      0      0

##### Sprawdzenie ile jest pustych wartości razem:

``` r
sumCountNa <- sum(countNa)
sumCountNa
```

    ## [1] 11056

##### Sprawdzenie ile jest wierszy z pustymi wartościami. Wzięliśy pod uwagę tylko prawdopodobne kolumny:

``` r
countBlackRows <- function(data) {
  blankRows <- data %>% filter(is.na(cfin1) | is.na(cfin2) | is.na(chel1) | is.na(chel2) | is.na(lcop1) | is.na(lcop2) | is.na(sst))
  blankRowsNumber <- count(blankRows)
  blankRowsNumber
}

blankRowsNumber <- countBlackRows(herrings)
blankRowsNumber
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 10094

Tym razem Wyszło nam mniej niż w przypadku zsumowania wszytkich zer.
Różnica ta wynika z tego, że niektóre wiersze mają więcej pustych
wartości niż jedną.

##### Sprawdzenie ile procent całości zajumją :

``` r
percentage <- blankRowsNumber/dimentions[1] * 100
round(percentage, 2)
```

    ##      n
    ## 1 19.2

Na podstaiwe powyższych obserwacji, zdecydowaliśmy się nie usuwać
wierszy z pustymi wartościami, ponieważ usuniemy wtedy aż 19.2% całości
danych. Taka ilość usuniętych danych na pewno wpłynęła by na wynik
analizy zbioru.

Ponieważ wartości z kolumn z ubytkami powtarzają się wielokrotnie,
postanowiliśmy zastosować prostą technikę, polegającą na zastąpieniu
brakujących danych, wartościami sąsiednimi.

##### Zastąpienie ubytków w danych sąsiednią wartością:

``` r
completeData <- herrings %>% mutate(
  cfin1 = case_when((is.na(cfin1) & !is.na(lag(cfin1))) ~ lag(cfin1), is.na(cfin1) ~ lead(cfin1), TRUE ~ cfin1),
  cfin2 = case_when((is.na(cfin2) & !is.na(lag(cfin2))) ~ lag(cfin2), is.na(cfin2) ~ lead(cfin2), TRUE ~ cfin2),
  chel1 = case_when((is.na(chel1) & !is.na(lag(chel1))) ~ lag(chel1), is.na(chel1) ~ lead(chel1), TRUE ~ chel1),
  chel2 = case_when((is.na(chel2) & !is.na(lag(chel2))) ~ lag(chel2), is.na(chel2) ~ lead(chel2), TRUE ~ chel2),
  lcop1 = case_when((is.na(lcop1) & !is.na(lag(lcop1))) ~ lag(lcop1), is.na(lcop1) ~ lead(lcop1), TRUE ~ lcop1),
  lcop2 = case_when((is.na(lcop2) & !is.na(lag(lcop2))) ~ lag(lcop2), is.na(lcop2) ~ lead(lcop2), TRUE ~ lcop2),
  sst = case_when((is.na(sst) & !is.na(lag(sst))) ~ lag(sst), is.na(sst) ~ lead(sst), TRUE ~ sst)
)


blankRowsNumber <- countBlackRows(completeData)
blankRowsNumber
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     9

Widzimy, że zastąpienie wartości uzupełniło nam większość danych, ale
nadal mamy 9 komórek bez wartości. Taką liczbę możemy już spokojnie
usunąć.

##### Usunięcie pozostałych wierszy:

``` r
completeData <- completeData %>% filter_all(all_vars(!is.na(.)))

blankRowsNumber <- countBlackRows(completeData)
blankRowsNumber
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     0

Teraz już mamy kompletne dane, bez pustych wartości. Możemy zatem
kontynuaować
przetwarzanie.

###### Zauważyliśmy, że większość danych w ramach jednego łowiska są identyczne, więc pogrupowaliśmy je po tych danych:

``` r
groupedData <- completeData %>% group_by(cfin1, cfin2, chel1, chel2, lcop1, lcop2, fbar, recr, cumf, totaln, sst, sal, nao)
groupedData
```

    ## # A tibble: 52,573 x 16
    ## # Groups:   cfin1, cfin2, chel1, chel2, lcop1, lcop2, fbar, recr, cumf, totaln,
    ## #   sst, sal, nao [557]
    ##        X length cfin1 cfin2 chel1 chel2 lcop1 lcop2  fbar   recr  cumf totaln
    ##    <int>  <dbl> <fct> <fct> <fct> <fct> <fct> <fct> <dbl>  <int> <dbl>  <dbl>
    ##  1     0   23   0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  2     1   22.5 0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  3     2   25   0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  4     3   25.5 0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  5     4   24   0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  6     5   22   0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  7     6   24   0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  8     7   23.5 0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ##  9     8   22.5 0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ## 10     9   22.5 0.02… 0.27… 2.46… 21.4… 2.54… 26.3… 0.356 482831 0.306 2.67e5
    ## # … with 52,563 more rows, and 4 more variables: sst <fct>, sal <dbl>,
    ## #   xmonth <int>, nao <dbl>

W taki sposób uzyskaliśmy pogrupowane dane do 557 wierszy, liczba ta
powinna odpowiadać iości łowisk.

##### Funkcja mapująca indeks na rok:

``` r
mapIndexToYear <- function(index) {
  maxIndex <- dim(herrings)[1]
  
  year = (index*60)/maxIndex
  as.integer(year)
  
}

mapIndexToYear(40000)
```

    ## [1] 45

##### Wykres zależności rozmiaru śledzia w analizowanych latach:

``` r
#completeData[["X"]]
plot(mapIndexToYear(groupedData[["X"]]), groupedData[["length"]], cex = 0.5, main = "Wykres zależności rozmiaru śledzia w analizowanych latach", xlab = "Kolejne lata", ylab = "Rozmiar śledzia")
```

![](Herringmania_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Rozkład wartości kolumn

##### Stworzenie wykresu rozkładu dla każdej ze zmiennych:

``` r
legend <- c(
    "length: długość złowionego śledzia [cm]",
    "cfin1: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1]",
    "cfin2: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2]",
    "chel1: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1]",
    "chel2: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2]",
    "lcop1: dostępność planktonu [zagęszczenie widłonogów gat. 1]",
    "lcop2: dostępność planktonu [zagęszczenie widłonogów gat. 2]",
    "fbar: natężenie połowów w regionie [ułamek pozostawionego narybku]",
    "recr: roczny narybek [liczba śledzi]",
    "cumf: łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]",
    "totaln: łączna liczba ryb złowionych w ramach połowu [liczba śledzi]",
    "sst: temperatura przy powierzchni wody [°C]",
    "sal: poziom zasolenia wody [Knudsen ppt]",
    "xmonth: miesiąc połowu [numer miesiąca]",
    "nao: oscylacja północnoatlantycka [mb]")

names <- names(groupedData)
df <- sapply(groupedData[, c(1:16)], as.numeric)

for (x in c(1:4)){
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  for (i in c(1:4)) {
    hist(df[,i*x],
    main= paste(" ",legend[i*x]),
    cex.main = 0.7,
    xlab=names[i*x],
    ylab = "% Liczności",
    col="darkmagenta",
    freq=FALSE
    )
  }
}
```

![](Herringmania_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](Herringmania_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](Herringmania_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](Herringmania_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

Niektóre rozkłady danych z jednej kategorii (kolumny) przypominają
rozkład normlany mi. dane: dostępności planktonu, miesiącu połowy czy
oscylacji północnoatlantyckiej. Wśród danych możemy też spotkać kolumny
które mają cechy wspomnianego rozkładu normalnego, lecz ciężko je nazwać
podobnymi do niego. Są to mi. dostepność plankltonu, natężenie połowów w
regionie, łączna liczba złowionych ryb, temperatura przy powierzchni
wody.

## Korelacja pomiędzy zmiennym

Ważnym czynnikiem jest także zbadanie korelacji pomiędzy poszczególnym,i
zmiennymi

``` r
res <- round(cor(df),2)
print(ggcorrplot(res, method = "circle"))
```

![](Herringmania_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
Powyższa macierz korelacji przedstawia zależności pomiędzy każdą parą
zmiennych. Przyjęty próg ważności obserwacji wynosi \< -0.4 oraz \> 0.4.
Przefiltrujmy macierz i zobaczmy które z par należą do przedziału
ważności.

##### Filtracja głównie-skorelowanych zmiennych macierzy korelacji:

``` r
pos_cor <- c(sort(unique(res[res > 0.4 & res != 1.0])))
neg_cor <- c(sort(unique(res[res < -0.4 & res != 1.0])))
```

Znalezione wartości korelacji pozytywnej przekraczające próg:

``` r
pos_cor
```

    ## [1] 0.41 0.44 0.56 0.73 0.77 0.82

Znalezione wartości korelacji negatywnej przekraczające próg:

``` r
neg_cor
```

    ## [1] -0.71 -0.51 -0.45

Chcielbyśmy zobaczyć które pary zmiennych kryją się za wysokimi
wartościami korelacji. W tym celu tworzymy funkcję która znajdzie parę
w macierzy korelacji.

``` r
show_pairs <- function(cor) {
  pairs <- matrix(ncol = 2)
  for (i in cor){
    a = which(res==i, arr.ind=TRUE)
    pairs = rbind(pairs, c(rownames(a)[1],rownames(a)[2]))
  }
  pairs = pairs[-1,]
  return(pairs)
}

positive <- show_pairs(pos_cor)
negative <- show_pairs(neg_cor)
```

Znalezione pary pozytywnej korelacji:

``` r
positive
```

    ##      [,1]    [,2]   
    ## [1,] "nao"   "X"    
    ## [2,] "cfin2" "cfin1"
    ## [3,] "nao"   "sst"  
    ## [4,] "lcop1" "chel1"
    ## [5,] "lcop2" "chel2"
    ## [6,] "cumf"  "fbar"

Znalezione pary negatywnej korelacji:

``` r
negative
```

    ##      [,1]     [,2]    
    ## [1,] "totaln" "cumf"  
    ## [2,] "totaln" "fbar"  
    ## [3,] "sst"    "length"

##### Analiza skorelowanych elementów

##### Przegląd najbardziej skorelowanych par zmiennych:

Pomijając autokorelację zmiennych (widoczną na przekątnej) z wykresu
możemy zaobserwować wyróżniające się pary dodatniej korelacji: 1. Wsp.
kor: 0.40 cfin1:cfin2 (dostępność planktonu \[zagęszczenie Calanus
finmarchicus gat. 1\] - dostępność planktonu \[zagęszczenie Calanus
finmarchicus gat. 2\];) 2. Wsp. kor: 0.41 nao:X (oscylacja
północnoatlantycka - indeks obserwacji) 3. Wsp. kor: 0.52 nao:sst
(oscylacja północnoatlantycka - temperatura przy powierzchni wody) 4.
Wsp. kor: 0.64 lcop1:chel1 (dostępność planktonu \[zagęszczenie
widłonogów gat. 1 - zagęszczenie Calanus helgolandicus gat. 1\] 5.
Wsp. kor: 0.69 lcop2:chel2 (dostępność planktonu \[zagęszczenie
widłonogów gat. 2 - zagęszczenie Calanus helgolandicus gat. 2\] 6.
Wsp. kor: 0.81 cumf:fbar (łączne roczne natężenie połowów w regionie
\[ułamek pozostawionego narybku\] - natężenie połowów w regionie
\[ułamek pozostawionego narybku\])

##### Ocena korelacji:

1.  Dostępność planktonu Calanus finmarchicus gat 1 jest zależna od
    planktonu tego samego rodzaju planktonu gat. 2.
2.  Wzmożona oscylacja północnoatlantycka jest powiązana ze wzrostem
    indeksu obserwacji. Korzystając z wiedzy, że obserwacje posortowane
    są chronologicznie można stwierdzić że oscylacja rośnie wraz z
    czasem.
3.  Wzmożona oscylacja północnoatlantycka jest powiązana ze wzrostem
    temperatury wody przy powierzchni.
4.  Dostępność planktonu widłonogów gat 1. jest zależna od występnowania
    helgolandicus gat. 1. Gatunki te często występują razem.
5.  Analogicznie jak pkt. 4 w przypadku gatunku nr 2 obu planktonów.
6.  Natężenie regionalne jest składową całościowego natężenia stąd
    wysoka korelacja jest czymś spodziewanym.

Wyróżniające się pary ujemnej korelacji: 7. Wsp. kor: -0.42 sst:length
(temperatura przy powierzchni wody - długość złowionego śledzia) 8. Wsp.
kor: -0.50 totaln:fbar (łączna liczba ryb złowionych w ramach połowu -
natężenie połowów w regionie) 9. Wsp. kor: -0.70 totaln:cumf (łączna
liczba ryb złowionych w ramach połowu - łączne roczne natężenie połowów
w regionie \[ułamek pozostawionego narybku\])

Ocena źródła korelacji: 7. Im temperatura wody jest cieplejsza tym
wyławiany śledź jest mniejszy. Analogicznie gdy tempeatura wody spada,
średnia długość śledzia rośnie. 8. Im większe regionalne natężenie
połowów w regionie, tym mniejsza jest liczba ryb łowionych w ramach
połowu. 9. Im większe całkowite natężenie połowów w regionie, tym
mniejsza jest liczba ryb łowionych w ramach połowu.

##### Filtracja zmiennych znacząco skorelowanych:

Można sprawdzić także jak wyglądają nieco mniej skorelowane wartości np.
w przedziale (-0.4:-0.3) or (0.3:0.4):

``` r
pos_cor <- c(sort(unique(res[res > 0.3 & res < 0.4])))
neg_cor <- c(sort(unique(res[res < -0.3 & res > -0.4])))

softly_pos <- show_pairs(pos_cor)
softly_neg <- show_pairs(neg_cor)
```

Znalezione pary pozytywnej korelacji:

``` r
softly_pos
```

    ##      [,1]     [,2]   
    ## [1,] "cumf"   "cfin2"
    ## [2,] "lcop2"  "cfin2"
    ## [3,] "sst"    "X"    
    ## [4,] "totaln" "recr"

Znalezione pary negatywnej korelacji:

``` r
softly_neg
```

    ##      [,1]     [,2]    
    ## [1,] "nao"    "totaln"
    ## [2,] "totaln" "X"     
    ## [3,] "nao"    "lcop2" 
    ## [4,] "length" "X"     
    ## [5,] "nao"    "chel2"

##### Ocena korelacji:

Przeglądając zbiór nieco mniej skorelowanych zmiennych można zauważyć,
że średnia temperatura wody rośnie z czasem. Z drugiej strony liczba
wyławianych śledzi jak i ich długość maleje z czasem. Ponadto rosnąca
oscylacja atlantycka ma wpływ na zmniejszenie się zagęszczenia planktonu
widłonogów gat. 2, Calanus helgolandicus gat. 2 oraz liczby wyławianych
śledzi.

## Konstrukcja regresora

``` r
#TODO
```

## Ewaluacja wyników miara R^2 i RMSE

``` r
#dumb data
obs <- 1:5
mod <- c(0.8,2.4,2,3,4.8)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rmse <- function(m, o) sqrt(mean((m - o)^2))

R2 <- rsq(obs, mod)
RMSE_v <- rmse(mod, obs)
```

Wynik ewaluacji miarą R^2

``` r
round(R2,3)
```

    ## [1] 0.856

Wynik ewaluacji miarą RSME

``` r
round(RMSE_v,3)
```

    ## [1] 0.669
