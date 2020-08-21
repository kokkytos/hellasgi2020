# Γλώσσα προγραμματισμού R: υπολογισμός δεικτών με δεδομένα νυχτερινών φώτων και οπτικοποίηση σε διαγράμματα και χάρτες. 

**[Εργαστήριο Χωρικής Ανάλυσης, Γεωγραφικών Πληροφοριακών Συστημάτων και Θεματικής Χαρτογραφίας](http://www.gislab.gr/)**
Το τρέχον αποθετήριο αποτελεί το υλικό για την παρουσίαση του εργαστηρίου (workshop) στο συνέδριο HellasGI 2020.

## Στόχος

Στόχος του εργαστηρίου είναι να παρουσιαστούν στον χρήστη οι δυνατότητες που προσφέρει η γλώσσα προγραμματισμού R για τον υπολογισμό δεικτών με τα δεδομένα νυχτερινών φώτων VIIRS Nighttime Day/Night Band Composites. 
Με πρακτικά παραδείγματα, θα υπολογιστεί, και θα οπτικοποιηθεί σε διαγράμματα και χάρτες ο δείκτης Sum of Lights (SoL) ανά μήνα και έτος για ένα πλήθος περιοχών.
Κατά την διάρκεια του εργαστηρίου θα επιδειχθεί μια σειρά αναγκαίων διαδικασιών τόσο επί των διανυσματικών όσο και επί των ψηφιδωτών δεδομένων.

## Πηγές δεδομένων

NOAA, National Geophysical Data Center, Defense Meteorological Satellite Program (DMSP). “Version 1 Nighttime VIIRS Day/Night Band Composites.” Earth Observation Group - Defense Meteorological Satellite Progam, Boulder | Ngdc.Noaa.Gov. Πρόσβαση 08/2020. https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html.


## Προαπαιτούμενα 

### Εκτέλεση του κώδικα του εργαστηρίου σε τοπικό επίπεδο

Για την εκτέλεση του κώδικα απαιτείται η λήψη του αποθετήριο μέσω του git (ή μέσω λήψης ως [συμπιεσμένο αρχείο](https://github.com/kokkytos/hellasgi2020/archive/master.zip).)
και τα παρακάτω προαπαιτούμενα:

* το πακέτο στατικής ανάλυσης **R** (https://www.r-project.org/) 
* το περιβάλλον εργασίας **Rstudio** (https://www.rstudio.com/)
* οι βιβλιοθήκες *raster, rasterVis, sf, stringr, furrr, tibble, dplyr, ggplot2, zoo*. Για την εγκατάστασή τους δώστε στο Console του rstudio την εντολή:
`install.packages(c("raster","rasterVis","sf","stringr","furrr","tibble","dplyr","ggplot2","zoo"),dependencies=T)`

* R Notebook vector και raster δεδομένα από το τρέχον αποθετήριο του workshop.
* Προαιρετικά το **git** (https://git-scm.com/). Για να λάβετε το περιεχόμενο του workshop μέσω του *git* δώστε την εντολή: `git clone https://github.com/kokkytos/hellasgi2020.git`

Το πλήρες περιβάλλον στο οποίο εκτελέστηκε ο τρέχων κώδικας R περιγράφεται στο αρχείο [sessionInfo.txt](sessionInfo.txt) 

### Εκτέλεση του κώδικα σε διαδραστικό διαδικτυακό περιβάλλον

Ο κώδικας του εργαστηρίο διατίθεται και διαδικτυακά με δυνατότητα διαδραστικής εκτέλεσης από τον χρήστη: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kokkytos/hellasgi2020/master?filepath=workshop.R)

## Συγγραφείς

* [Λιάκος Λεωνίδας](https://gr.linkedin.com/in/leonidasliakos)

* [Σταθάκης Δημήτρης](https://gr.linkedin.com/in/dstath)


## Χρηματοδότηση

«Το έργο συγχρηματοδοτείται από την Ελλάδα και την Ευρωπαϊκή Ένωση (Ευρωπαϊκό Κοινωνικό Ταμείο) μέσω του Επιχειρησιακού Προγράμματος «Ανάπτυξη Ανθρώπινου Δυναμικού, Εκπαίδευση και Διά Βίου Μάθηση», στο πλαίσιο της Πράξης «Ενίσχυση του ανθρώπινου ερευνητικού δυναμικού μέσω της υλοποίησης διδακτορικής έρευνας» (MIS-5000432), που υλοποιεί το Ίδρυμα Κρατικών Υποτροφιών (ΙΚΥ)»
