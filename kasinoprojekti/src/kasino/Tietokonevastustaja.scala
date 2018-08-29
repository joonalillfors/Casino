package kasino

import collection.mutable.{Buffer, Set}

/*
 *  Tietokonevastustaja perii luokan Pelaaja, tämän ilmentymä osaa tehdä itsenäisesti siirtoja pelissä
 */
class Tietokonevastustaja(nimi: String, kortitKadessa: Buffer[Kortti], pino: Buffer[Kortti] = Buffer[Kortti](), mokit: Int = 0, pisteet: Int = 0) extends Pelaaja(nimi, kortitKadessa, pino, mokit, pisteet) {
  
  def subsets = this.kierros.get.kortitPoydalla.to[Set].subsets.toArray
  
  // Tietokone yrittää ottaa pöydästä mahdollisimman monta korttia, jos ei pysty nostamaan pöydältä, katsoo onko kädessä samanarvoisia,
  // jos on niin laittaa yhden niistä, muuten pienimmän kortin pöytään
  def strategia: Boolean = {
    val sopivat = kortitKadessa.flatMap( x => this.subsets.filter(x.isLegal) )
    if (kortitKadessa.isEmpty) {
      kierros.get.laskuri += 1
      true
    } else if (sopivat.isEmpty) {
      val samat = kortitKadessa.filter(x => x.arvoKadessa == x.arvoPoydalla).groupBy(_.arvoPoydalla).find(_._2.size > 1)
      if (samat.isDefined) 
        this.laitaKortti(samat.get._2.head)
      else
        this.laitaKortti(kortitKadessa.minBy(_.arvoKadessa))                        // laitetaan pienin kortti kädessä pöytään
      true
    } else {
      val poydalta = sopivat.maxBy(_.size)                              // valitaan eniten kortteja
      val kadesta = kortitKadessa.find(_.isLegal(poydalta)).get
      this.otaKortteja(kadesta, poydalta)
    }
  }
  
}