package kasino.gui

import kasino._
import collection.mutable.{Buffer, Set}
import swing._
import javax.swing.ImageIcon
import io._
import java.io._
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

object Kayttoliittyma extends SimpleSwingApplication {
  
  val images = ImageIO.read(new File("cards.png"))
  
  case class Sprite(id: String, image: BufferedImage)

  def getSprites(imageFile: String) = {

    // Luetaan kuvatiedosto 
    val spriteSheet = ImageIO.read(new File("cards.png"))
    
    (for {
      a <- 1 to 13
      m <- 1 to 4
      image = spriteSheet.getSubimage((a-1)*73, (m-1)*98, 73, 98)
      id = m+":"+a
    } yield id -> Sprite(id,image)).toMap
    
  }

  var pelikierros: Option[Pelikierros] = None
  
  val width = 800
  val height = 600
  
  def top = new MainFrame {
    title = "Kasino"
    minimumSize = new Dimension(width, height)
    preferredSize = new Dimension(width, height)
    maximumSize = new Dimension(width, height)
    val sprites = getSprites("cards.png")
    
    menuBar = new MenuBar {
      contents += new Menu("Valikko") {
        contents += new MenuItem(Action("Alkuun") {
          startScreen
        })
        contents += new Separator
        contents += new MenuItem(Action("Tallenna peli") {
          pelikierros.foreach(_.tallennaPeli("tallennus.txt"))
        })
        contents += new Separator
        contents += new MenuItem(Action("Sulje") {
          quit()
        })
      }
    }
    
    // Pelin lopettava ruutu, missä kerrotaan pelin voittaja ja pelaajien pisteet
    def endScreen(kierros: Pelikierros) = {
      contents = new BoxPanel(Orientation.Vertical) {
        override def paintComponent(g: Graphics2D) = {
          g.setColor(new Color(10,100,50))
          g.fillRect(0,0,width,height)
          g.setColor(Color.red)
          g.setFont(new Font("Arial", 1, 20))
          g.drawString("Voittaja", 300, 100)
          g.drawString(kierros.pelaajat.maxBy(_.pisteet).nimi, 300, 150)
          g.drawString("Pelaaja", 300, 250)
          g.drawString("Pisteet", 500, 250)
          kierros.pelaajat.sortBy(-_.pisteet).zipWithIndex.foreach(x => {
            g.drawString(x._1.nimi, 300, 300+x._2*20)
            g.drawString(x._1.pisteet.toString, 500, 300+x._2*20)
          })
        }
      }
    }
    
    // Pelitilanteen piirtävä metodi
    def game(kierros: Pelikierros): Unit = {
      pelikierros = Some(kierros)
      val poydalta = Buffer[Kortti]()
      
      def update(): Unit = {
        poydalta.clear
        // Katsoo onko pelikierros ohitse
        if (kierros.tyhjaPakka && kierros.pelaajat.forall(_.kortitKadessa.isEmpty)) {
          if (kierros.pelaajat.exists(_.pisteet >= 16)) {
            endScreen(kierros)
          } else {
            kierros.pelaajat((kierros.laskuri-1) % kierros.pelaajat.size).pino ++= kierros.kortitPoydalla
            kierros.kortitPoydalla.clear()
            kierros.lisaaPisteet
            if (kierros.pelaajat.exists(_.pisteet >= 16))
              endScreen(kierros)
            else {
              val peli = Peli.uusiPeli(kierros.pelaajat.map(x => x.isInstanceOf[Tietokonevastustaja] -> x.nimi).toBuffer)
              peli.pelaajat.foreach(x => x.pisteet = kierros.pelaajat.find(_.nimi==x.nimi).get.pisteet)
              peli.jakaja = peli.pelaajat((peli.pelaajat.indexOf(peli.jakaja)+1) % peli.pelaajat.size)
              peli.laskuri = peli.pelaajat.indexOf(peli.jakaja)+1
              println("uusi pelikierros")
              game(peli)
            }
          }
        } else { // Itse pelitilanne
          contents = new BoxPanel(Orientation.Vertical) {
            
            val poyta = new GridPanel(1,kierros.kortitPoydalla.size) {
            maximumSize = new Dimension(width, 100)
            val cardss = new ButtonGroup {
              maximumSize = new Dimension(width, 100)
              kierros.kortitPoydalla.foreach(x => {
                contents += new CheckBox {
                  action = Action("") {
                    if (poydalta.contains(x)) poydalta -= x
                    else poydalta += x
                  }
                  borderPainted = true
                  val sprite = sprites(x.maa+":"+x.arvoPoydalla)
                  icon = new ImageIcon(sprite.image)
                }
              })
            }
            contents ++= cardss.buttons
            }
            
            contents += poyta
  
            contents += new Panel {
              override def paintComponent(g: Graphics2D) = {
                  g.setColor(new Color(10,100,50))
                  g.fillRect(0,0,width,height)
                  g.setColor(Color.red)
                  g.setFont(new Font("Arial", 1, 20))
                  g.drawString(kierros.vuoro.nimi, 400, 300)
              }
            }          
            
            var kadesta: Option[Kortti] = None
            
            val kasi = new GridPanel(1,4){ 
              maximumSize = new Dimension(width, 100)
              if (kierros.vuoro.kortitKadessa.isEmpty) kierros.laskuri += 1
              else if (!kierros.vuoro.isInstanceOf[Tietokonevastustaja]) {
                val cardss = new ButtonGroup(kierros.vuoro.kortitKadessa.map(x => {
                    new RadioButton("") {
                      action = Action("") {
                        kadesta = Some(x)
                      }
                      borderPainted = true
                      val sprite = sprites(x.maa+":"+x.arvoPoydalla)
                      icon = new ImageIcon(sprite.image)
                    }
                  }):_* )
                contents ++= cardss.buttons
              }
          }
          contents += kasi
          
                    contents += new GridPanel(1,2) {
              maximumSize = new Dimension(width,50)
              contents += new Button(Action("Tietokoneen siirto") {
                if (kierros.vuoro.isInstanceOf[Tietokonevastustaja]) {
                  kierros.vuoro.asInstanceOf[Tietokonevastustaja].strategia
                  update
                }
              })
              contents += new Button(Action("Ota/laita kortti") {
                if (!kierros.vuoro.isInstanceOf[Tietokonevastustaja]) {
                  if (poydalta.isEmpty && kadesta.isDefined) {
                    kadesta.foreach(kierros.vuoro.laitaKortti)
                  } else if (kadesta.isDefined) {
                    var siirto = kierros.vuoro.otaKortteja(kadesta.get, poydalta.to[Set])
                    if (!siirto) println("Laiton siirto")
                  }
                  update
                }
              })
            }
          }
        }
      }
      update()
    }
    
    // Uuden pelin alustus, pelaajien lisäys peliin
    def newGame(): Unit = {
      val pelaajat = Buffer[(Boolean, String)]()
      contents = new BoxPanel(Orientation.Vertical) {
        val paneeli = new Panel {
          override def paintComponent(g: Graphics2D) = {
            g.setColor(new Color(10,100,50))
            g.fillRect(0,0,width, height)
            g.setColor(Color.black)
            g.drawString("Kasino", width/2-20, height/4)
            g.setColor(Color.red)
            
            pelaajat.zipWithIndex.foreach(x => g.drawString(x._1._2, 10, 10*(x._2+1)))
          }
        }
        contents += paneeli
        contents += new GridPanel(6,1) {
          maximumSize = new Dimension(width, width+200)
          contents += new Button(Action("Reset") {
            newGame
          })
          
          val rb = new RadioButton("Tietokonevastustaja")
          rb.selected = true
          val plr = new RadioButton("Pelaaja")
          contents ++= new ButtonGroup(rb, plr).buttons
          
          var nimi = ""
          val nameArea = new TextField(name)
          contents += nameArea
          
          contents += new Button(Action("Lisää pelaaja") {
            if (nimi.isEmpty) println("Syötä pelaajan nimi")
            else if (pelaajat.size >= 8) println("Max. 8 pelaajaa")
            else {
              if (plr.selected) {
                if (pelaajat.exists(_._2 == nimi)) {nimi += "1"; pelaajat += false -> (nimi)}
                else pelaajat += (false -> nimi)
              }
              else {
                if (pelaajat.exists(_._2 == nimi)) {nimi += "1"; pelaajat += true -> (nimi)}
                else pelaajat += (true -> nimi)
              }
            }
            paneeli.repaint
          })
          
          contents += new Button(Action("Aloita peli") {
            if (pelaajat.size < 2) println("Syötä pelaajia")
            else game(Peli.uusiPeli(pelaajat))
          })
          listenTo(nameArea)
          reactions += {
            case event.EditDone(nameArea) => nimi = nameArea.text
          }
        }
      }
    }
    
    // Pelin lataus tiedostosta
    def loadGame(): Unit = {
      try{
        game(Peli.lataaPeli("tallennus.txt"))
      }
      catch {
        case e:FileNotFoundException => {
          println("Ei tallennettua peliä")
          startScreen
        }
      }
    }
    
    // Aloitusruutu
    def startScreen() = {
      pelikierros = None
      contents = new BoxPanel(Orientation.Vertical) {
        contents += new Panel {
          override def paintComponent(g: Graphics2D) = {
            g.setColor(new Color(10,100,50))
            g.fillRect(0,0,width, height)
            g.setColor(Color.black)
            g.drawString("Kasino", width/2-20, height/4)
          }
        }
        contents += new GridPanel(2,1) {
          contents += new Button(Action("Uusi peli") {
            newGame
          })
          contents += new Button(Action("Lataa peli") {
            loadGame
          })
          maximumSize = new Dimension(width,width)
        }
      }
      
    }
    
    // Kutsutaan aluksi aloitusruutua
    startScreen
  }
  
}