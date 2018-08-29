package kasino

import collection.mutable.{Buffer, Stack, Map}
import io._
import java.io._

/*
 * Luokan ilmentymä kuvaa yksittäistä pelikierrosta pelissä, sillä on tiedossa pelin pelaajat, jakaja, pöydällä olevat kortit,
 * pakassa olevat kortit, sekä vuorolaskuri 
 */
class Pelikierros(val pelaajat: Vector[Pelaaja], var jakaja: Pelaaja, var kortitPoydalla: Buffer[Kortti], private var korttipakka: Stack[Kortti], var laskuri: Int = -1) {
  
  // Lisätään jokaiselle kierroksella olevalle pelaajalle tieto kierroksella olemisesta
  pelaajat.foreach(_.kierros=Some(this))
  
  // Laskurille annetaan alkuarvo vain ladattaessa peliä tiedostosta, muuten laskurin arvoksi asetetaan jakajasta seuraava (alkuarvoisesti jakaja on ensimmäinen pelaaja)
  if (laskuri == -1) laskuri = pelaajat.indexOf(jakaja)+1
  
  // Palauttaa kenen pelaajan vuoro on
  def vuoro: Pelaaja = {
    pelaajat(laskuri % pelaajat.size)
  }
  
  // Nostetaan palauttaa pakasta päällimmäisen kortin ja poistaa sen pakasta
  def nostaKortti: Kortti = korttipakka.pop()
  
  // Palauttaa totuusarvon, onko korttipakka tyhjä
  def tyhjaPakka: Boolean = korttipakka.isEmpty
  
  // Laskee pelaajien pisteet kuluvalta kierrokselta
  def laskePisteet: Map[Pelaaja, Int] = {                                                             // lasketaan kullekin pelaajalle kierroksella saadut pisteet
    var pistemap = Map(pelaajat.map( x => x -> x.pisteetKierroksella ):_*)              // tehdään mutable pistemap pelaajien pisteille ja lisätään seuraavaksi 
    pistemap(pelaajat.map( x => x -> x.pino.filter( _.maa == 2).length ).toArray.maxBy( _._2 )._1) += 2      // lisätään eniten patoja saaneelle pelaajalle 2 pistettä 
    pistemap(pelaajat.map( x => x -> x.pino.length ).toArray.maxBy( _._2 )._1) += 1                        // lisätään eniten kortteja saaneelle pelaajalle 1 piste
    pistemap
  }
  
  // Lisää pelaajien saamat pisteet kuluvalta kierrokselta pelaajien kokonaispisteisiin
  def lisaaPisteet(): Unit = {
    val pisteet = laskePisteet
    pisteet.foreach( x => x._1.pisteet += x._2 )                              // lisätään kierroksella saadut pisteet pelaajien kokonaispisteisiin
  }
  
  // Tallentaa pelitilanteen tekstitiedostoon
  def tallennaPeli(tiedostonimi: String) = {
    val file = new File(tiedostonimi)
    val bw = new BufferedWriter(new FileWriter(file))
    def writeCard(x: Kortti) = bw.write(x.arvoPoydalla+","+x.arvoKadessa+","+x.maa+"\n")
    
    bw.write("#kortit poydalla\n")
    kortitPoydalla.foreach(writeCard(_))
    
    bw.write("#korttipakka\n")
    korttipakka.foreach(writeCard(_))
    
    pelaajat.foreach( pelaaja => { 
      bw.write("#pelaaja\n")
      bw.write("%nimi:"+pelaaja.nimi+"\n")
      bw.write("%mokit:"+pelaaja.moekit+"\n")
      bw.write("%pisteet:"+pelaaja.pisteet+"\n")
      if (jakaja == pelaaja) bw.write("%jakaja\n")
      if (pelaaja.isInstanceOf[Tietokonevastustaja]) bw.write("%tietokonevastustaja\n")
      bw.write("%pino\n")
      pelaaja.pino.foreach(writeCard(_))
      bw.write("%kadessa\n")
      pelaaja.kortitKadessa.foreach(writeCard(_))
    })
    
    bw.write("#laskuri\n")
    bw.write(laskuri+"\n")
    
    bw.close
  }
  
}