package kasino

import collection.mutable.{Buffer, Stack}
import util.Random
import io._
import java.io._

object Peli {
  
  // Lukee pelikierroksen tilanteen tiedostosta ja palauttaa Pelikierros-luokan ilmentymän joka vastaa tiedostossa olevaa pelitilannetta
  def lataaPeli(tiedostonimi: String) = {
    val lineReader = Source.fromFile(tiedostonimi).bufferedReader()
    var currentLine = lineReader.readLine()
    val headerParts = currentLine.split(" ")
    
    var laskuri = 0
    val pelaajat = Buffer[Pelaaja]()
    val poydalla = Buffer[Kortti]()
    var korttipakka = Stack[Kortti]()
    var jakaja: Option[Pelaaja] = None
    
    while (currentLine != null) {
      currentLine = currentLine.toLowerCase.trim
      currentLine match {
        case "#kortit poydalla" => {
          currentLine = lineReader.readLine()
          while (currentLine != null && !currentLine.startsWith("#")) {
            val kortti = currentLine.split(",").map(_.trim.toInt)
            poydalla += Kortti(kortti(0), kortti(1), kortti(2))
            currentLine = lineReader.readLine()
          }
        }
        case "#korttipakka" => {
          currentLine = lineReader.readLine()
          while (currentLine != null && !currentLine.startsWith("#")) {
            val kortti = currentLine.split(",").map(_.trim.toInt)
            korttipakka = korttipakka.push(Kortti(kortti(0), kortti(1), kortti(2)))
            currentLine = lineReader.readLine()
          }
          korttipakka = korttipakka.reverse                                          // käännetään pakka ympäri, jotta se pysyy alkuperäisessä järjestyksessä
        }
        case "#pelaaja" => {
          var nimi = ""
          var pino = Buffer[Kortti]()
          var mokit = 0
          var kadessa = Buffer[Kortti]()
          var pisteet = 0
          var dealer = false
          var tietokonevastustaja = false
          currentLine = lineReader.readLine()
          while (currentLine != null && !currentLine.startsWith("#")) {
            currentLine = currentLine.toLowerCase.trim
            val cur = currentLine.split(":").map(_.trim)
            cur.head match {
              case ("%nimi") => {
                nimi = cur(1)
                currentLine = lineReader.readLine()
              }
              case ("%mokit") => {
                mokit = cur(1).toInt
                currentLine = lineReader.readLine()
              }
              case ("%pisteet") => {
                pisteet = cur(1).toInt
                currentLine = lineReader.readLine()
              }
              case ("%jakaja") => {
                 dealer = true
                 currentLine = lineReader.readLine()
              }
              case ("%tietokonevastustaja") => {
                tietokonevastustaja = true
                currentLine = lineReader.readLine()
              }
              case ("%pino") => {
                currentLine = lineReader.readLine()
                while (currentLine != null && !currentLine.startsWith("#") && !currentLine.startsWith("%")) {
                  val kortti = currentLine.split(",").map(_.trim.toInt)
                  pino += Kortti(kortti(0), kortti(1), kortti(2))
                  currentLine = lineReader.readLine()
                }
              }
              case ("%kadessa") => {
                currentLine = lineReader.readLine()
                while (currentLine != null && !currentLine.startsWith("#") && !currentLine.startsWith("%")) {
                  val kortti = currentLine.split(",").map(_.trim.toInt)
                  kadessa += Kortti(kortti(0), kortti(1), kortti(2))
                  currentLine = lineReader.readLine()
                }
              }
              case _ => currentLine = lineReader.readLine()
            }
            
          }
          pelaajat += {
            if (tietokonevastustaja)
              new Tietokonevastustaja(nimi, kadessa, pino, mokit, pisteet)
            else
              new Pelaaja(nimi, kadessa, pino, mokit, pisteet)
          }
          if (dealer) jakaja = Some(pelaajat.last)
        }
        case "#laskuri" => {
          currentLine = lineReader.readLine()
          while (currentLine != null && !currentLine.startsWith("#")) {
            laskuri = currentLine.toInt
            currentLine = lineReader.readLine()
          }
        }
        case _ => currentLine = lineReader.readLine()
      }
      
    }
    new Pelikierros(pelaajat.toVector, jakaja.getOrElse(pelaajat.head), poydalla, korttipakka, laskuri)
  }
  
  // Alustaa uuden Pelikierros-luokan ilmentymän, luo ja sekoittaa korttipakan ja jakaa kortit pöydälle ja pelaajien käteen
  // Jos pelaajan totuusarvo on true => Tietokonevastustaja ja false => Pelaaja
  def uusiPeli(pelaajat: Buffer[(Boolean, String)]): Pelikierros = {
    val korttipakka = Stack[Kortti](Random.shuffle(          // alustetaan sekoitettu korttipakka
      for {
        arvo <- 1 to 13
        maa <- 1 to 4
      } yield {
        (maa, arvo) match {
          case (_, 1)   => new Kortti(1, 14, maa)
          case (4, 10)  => new Kortti(10, 16, maa)
          case (2, 2)   => new Kortti(2, 15, maa)
          case _        => new Kortti(arvo, arvo, maa)
        }
      }):_*)
    val plr = pelaajat.map(i => 
      if (i._1) 
        new Tietokonevastustaja(i._2, Buffer.tabulate(4)(x=>korttipakka.pop))
      else
        new Pelaaja(i._2, Buffer.tabulate(4)(x=>korttipakka.pop))
      ).toVector     // luodaan pelaaja-luokan ilmentymät ja alustetaan kullekkin 4 korttia käteen
    
    new Pelikierros(plr, plr.head, Buffer.tabulate(4)(x => korttipakka.pop()), korttipakka)
  }
  
}