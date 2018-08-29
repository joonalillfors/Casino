package kasino

import org.junit.Test
import org.junit.Assert._
import collection.mutable.Set

class UnitTests {
  
  // Testaa tekeekö tietokonevastustaja siirron
  @Test def testTietokonevastustaja() {
    val peli = Peli.uusiPeli(collection.mutable.Buffer((true, "boi")))
    val a = peli.pelaajat.map(_.asInstanceOf[Tietokonevastustaja].strategia)
    assertTrue(a.forall(_ == true))
  }
  
  @Test def testIsLegal() {
    val kortit = Set(Kortti(1, 14, 2), Kortti(1, 14, 3), Kortti(2, 2, 4), Kortti(4,4,2))
    val kadesta = Kortti(4, 4, 3)
    assertTrue(kadesta.isLegal(kortit))
  }
  
  @Test def testIsLegal2() {
    val kortit = Set(Kortti(4, 4, 4), Kortti(4,4,2), Kortti(3,3,2))
    val kadesta = Kortti(4, 4, 3)
    assertFalse(kadesta.isLegal(kortit))
  }
  
  @Test def testIsLegal3() {
    val kortit = Set(Kortti(4,4,4), Kortti(2,2,2), Kortti(2,2,3), Kortti(8,8,2), Kortti(8,8,3))
    val kadesta = Kortti(12,12,1)
    assertTrue(kadesta.isLegal(kortit))
  }
  
  @Test def testIsLegal4() {
    val kortit = Set(Kortti(10,10,2), Kortti(10,10,3))
    val kadesta = Kortti(10,10,1)
    assertTrue(kadesta.isLegal(kortit))
  }
  
}