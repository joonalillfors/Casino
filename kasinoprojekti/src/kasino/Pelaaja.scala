package kasino

import collection.mutable.{Buffer, Set}

/*
 *  Luokan ilmentymä kuvaa yksittäistä pelaajaa pelikierroksella, kullakin pelaajalla on tiedossa nimi, kädessä olevat kortit, pinossa olevat
 *  kortit, mökkien lukumäärä, sekä kertyneet pisteet
 */
class Pelaaja(val nimi: String, var kortitKadessa: Buffer[Kortti], val pino: Buffer[Kortti] = Buffer[Kortti](), private var mokit: Int = 0, var pisteet: Int = 0) {
  
  // Pelikierros, jolla pelaaja on, alkuarvoisesti None, mutta luodessa Pelikierros-ilmentymää alustetaan oikea arvo
  var kierros: Option[Pelikierros] = None
  
  // Palauttaa montako mökkiä pelaaja on saanut
  def moekit = this.mokit
  
  // Poistaa parametrin kortin pelaajan kädestä ja nostaa pakasta uuden kortin tilalle, mikäli pakassa on kortteja jäljellä,
  // lisää laskuriin, että pelaajan vuoro on päättynyt
  def poistaKorttiKadesta(kortti: Kortti) = {                              // jos kierroksen korttipakka ei ole tyhjä, lisää kädessä oleviin kortteihin poistettavan kortin tilalle korttipakan päällimmäisen kortin
    if (!kierros.get.tyhjaPakka)                        // muuten vain poistaa kortin kädestä
      kortitKadessa(kortitKadessa.indexOf(kortti)) = kierros.get.nostaKortti
    else
      kortitKadessa -= kortti
    kierros.get.laskuri += 1
  }
  
  // Jos pelaaja pystyy parametrillä kadesta ottamaan setin kortit poydalta, palauttaa true ja toteuttaa pelaajan siirron, muutoin palauttaa false
  def otaKortteja(kadesta: Kortti, poydalta: Set[Kortti]): Boolean = {
    require(kierros.isDefined)
    if (kadesta.isLegal(poydalta)) {                                                  // jos kädessä olevan korttien arvo on sama kuin pöydässä olevien arvojen summa
      require(poydalta.forall(x=>kierros.forall(_.kortitPoydalla.contains(x))))      // tai kierros.get.kortitPoydalla.contains(ulos)
      if (kierros.forall(_.kortitPoydalla.forall(poydalta.contains(_))))             // katsotaan otetaanko kaikki kortit pöydältä
        mokit += 1
      kierros.foreach(_.kortitPoydalla --= poydalta)                                  // poistetaan pöydältä otetut kortit pöydältä
      pino ++= poydalta.toVector :+ kadesta                                                    // lisätään kortit omaan pinoon
      poistaKorttiKadesta(kadesta)
      true
    } else false
  }
  
  // Laittaa parametrinä annetun kortin pelaajan kädestä pöydälle ja poistaa sen pelaajan kädestä
  def laitaKortti(kortti: Kortti): Unit = {
    require(kierros.isDefined)
    kierros.foreach(_.kortitPoydalla += kortti)                      // lisätään kortti pöydälle
    poistaKorttiKadesta(kortti)
  }
  
  // Laskee pelaajan mökit, ässät ja onko pelaajalla ruutu-10 tai pata-2 pinosta, palauttaa näistä kertyneet pisteet
  def pisteetKierroksella = {
    var pts = mokit + pino.filter( _.arvoPoydalla == 1 ).length
    if (pino.exists( x => x.maa == 4 && x.arvoPoydalla == 10 ))    // ruutu-10
      pts += 2
    if (pino.exists( x => x.maa == 2 && x.arvoPoydalla == 2 ))    // pata-2
      pts += 1
    pts
  }
  
  
}