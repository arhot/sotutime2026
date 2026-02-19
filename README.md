# Sosiaalitieteiden tilastomenetelmät – R-harjoitukset 2026

Tämä kansio sisältää kurssin R-harjoitustiedostot sekä koontitiedostot yleisimmistä analyyseista.
Aineistona käytetään European Social Survey -kyselyä (ESS 2023, Suomen otos).

---

## Tiedostot

### Harjoitustehtävät

| Tiedosto | Aihe |
|---|---|
| `sotutime_harjoitus1_2026.Rmd` | R- ja RStudion asennus, aineiston avaaminen, histogrammi ja hajontakuvio |
| `sotutime_harjoitus2_2026.Rmd` | Muuttujamuunnokset: ikälaskenta, summamuuttuja, puuttuvien poisto |
| `sotutime_harjoitus3_2026.Rmd` | Faktorianalyysi, faktoripistemuuttujat, taulukoiden rakentaminen |
| `sotutime_harjoitus4_2026.Rmd` | Lineaarinen regressio, mallin rakentaminen ja taulukointi |
| `sotutime_harjoitus5_2026.Rmd` | Luokitellut muuttujat: as_factor, fct_recode, fct_lump, cut, ristiintaulukko |
| `sotutime_harjoitus6_2026.Rmd` | Varianssianalyysi, yhdysvaikutus, post hoc -testit, keskiarvokuvio |

### Koontitiedostot

Nämä tiedostot kokoavat harjoitusten tärkeimmät koodit lyhyeen muotoon kotiin vietäväksi.

| Tiedosto | Sisältö |
|---|---|
| `tavallisimpien_muuttujamuunnosten_teko_sotutime.Rmd` | Uudelleennimeäminen, puuttuvien poisto, laskutoimitukset, summamuuttuja, standardointi, suunnan kääntö, logaritmimuunnos, luokitellut muuttujat, cut, case_when |
| `tavallisimpien_mallien_teko_sotutime.Rmd` | Faktorianalyysi, lineaarinen regressio, varianssianalyysi (ANOVA) |
| `tavallisimpien_visualisointien_teko_sotutime.Rmd` | Pylväskuvio, histogrammi, hajontakuvio, piste-ja-viiksi-kuvio (ANOVA) |

### Muut tiedostot

| Tiedosto | Kuvaus |
|---|---|
| `ESS2023_Suomi.sav` | Alkuperäinen ESS 2023 -aineisto (Suomi), SPSS-muodossa |
| `ESS2016.sav` | Vanhempi ESS 2016 -aineisto vertailukäyttöön |
| `app.R` | Shiny-sovellus |
| `sotutime2024.Rproj` | RStudio-projektitiedosto – avaa tämä RStudiossa |

---

## Aloittaminen

1. Avaa `sotutime2024.Rproj` RStudiossa (tämä asettaa työhakemiston oikein automaattisesti).
2. Asenna tarvittavat paketit ensimmäisellä kerralla (ohjeet harjoituksessa 1).
3. Etene harjoitukset järjestyksessä – myöhemmät harjoitukset käyttävät aiemmissa luotuja muuttujia.

### Tarvittavat paketit

```r
install.packages(c("tidyverse", "forcats", "haven", "janitor",
                   "psych", "lm.beta", "labelled", "broom",
                   "writexl", "sjstats", "pwr"))
```

Kurssipaketti `sostieteidentaulukointityylit` asennetaan erikseen kurssin ohjeiden mukaan.

---

## Aineiston lataaminen

```r
ESS2023_Suomi <- haven::read_sav("ESS2023_Suomi.sav")
```

Jos olet tallentanut muokatun aineiston aiemmalta harjoituskerralta:

```r
ESS2023_Suomi <- haven::read_sav("ESS2023_Suomi_editoitu.sav")
```
