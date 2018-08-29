package kasino

import collection.mutable.Set

/*
 * Maat: 1 = risti, 2 = pata, 3 = hertta, 4 = ruutu
 */
case class Kortti(arvoPoydalla: Int, arvoKadessa: Int, maa: Int) {
  
  // Tarkistetaan onko parametrinä annettujen korttien summa pöydällä yhtä suuri kuin this.arvoKadessa
  def isEqual(kortit: Set[Kortti]): Boolean = {
    kortit.toVector.map(_.arvoPoydalla).sum == this.arvoKadessa
  }
  
  // Tarkistetaan onko siirto laillinen eli voidaanko kortilla this ottaa pöytästä parametrina annetut kortit
  def isLegal(kortit: Set[Kortti]): Boolean = {
    // Etsitään subsetit jotka toteutta metodin isEqual
    val subsets = kortit.subsets.to[Set].filter(this.isEqual(_) == true)
    if (subsets.isEmpty) false
    else {
    // Etsitään onko subsetsillä osajoukkoa, jossa jokaista korttia olisi käytetty tasan kerran
    subsets.subsets.exists(x => x.flatten.size == kortit.size && x.flatten.size == x.toVector.map(_.size).sum)
    }
  }
  
}