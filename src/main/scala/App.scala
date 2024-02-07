package ec.edu.utpl.computacion.pf.pi
import com.github.tototoshi.csv.*
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData
import org.nspl.saddle.barplotVertical
import org.saddle.*
import org.saddle.order.*
import java.io.File

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main
  def pintegra() =
    val file: String = "C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\dsPartidosYGoles.csv"
    val reader = CSVReader.open(new File(file))
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    reader.close()

    val file2: String = "C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\dsAlineacionesXTorneo.csv"
    val reader2 = CSVReader.open(new File(file2))
    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()
    reader2.close()

    chartingEcuadorLocal(golesEcuadorLocal(contentFile))
    chartingEcuadorVisitante(golesEcuadorVisitante(contentFile))
    chartingAnioGoles(datosGraficaAnioGoles(contentFile))
    chartingDefensores(datosGraficaDefensasAñoDatos2(contentFile2, contentFile))
    chartingFaseDeGrupos(golesPorFaseDeGrupo(contentFile))
    chartingEstadios(datosGraficaCiudadEstadio(contentFile))

    /*
    ////IMPRIME EJERCICIO 1 EN CONSOLA/////
    val ecuLocal: List[(Int)] = golesEcuadorLocal(contentFile)
    ecuLocal.foreach(println)

    ////IMPRIME EJERCICIO 2 EN CONSOLA/////
    val ecuVisita: List[(Int)] = golesEcuadorVisitante(contentFile)
    ecuVisita.foreach(println)

    ////IMPRIME EJERCICIO 3 EN CONSOLA/////
    val datosAnioGoles: List[(Int, Int)] = datosGraficaAnioGoles(contentFile)
    datosAnioGoles.foreach(println)

    ////IMPRIME EJERCICIO 4 EN CONSOLA/////
    val datosDefesnsasAD: List[(Int, Int)] = datosGraficaDefensasAñoDatos2(contentFile2, contentFile)
    datosDefesnsasAD.foreach(println)

    ////IMPRIME EJERCICIO 5 EN CONSOLA/////
    val golesPorFaseGrupo: List[(String, Int)] = golesPorFaseDeGrupo(contentFile)
    golesPorFaseGrupo.foreach(println)

    ///IMPRIME EJERCICIO 6 EN CONSOLA/////
    val ciudadEstadio: List[(String, Int)] = datosGraficaCiudadEstadio(contentFile)
    ciudadEstadio.foreach(println)
    */


  /////////////////////GRAFICA 1 HISTOGRAMA//////////////
  def golesEcuadorLocal(data: List[Map[String, String]]): List[Int] = {
    val localEcuador = data.filter { matchData =>
      matchData("home_team_name") == "Ecuador"
    }
    val partidosLocalEcuador = localEcuador.groupBy(_("matches_match_id")).map(_._2.head)
    val golesPorPartido = partidosLocalEcuador.map(row => row("matches_home_team_score").toInt)
    golesPorPartido.toList
  }
  def chartingEcuadorLocal(data: List[Int]): Unit = {
    val histograma = xyplot(HistogramData(data.map(_.toDouble), 60) -> bar())(par
      .xlab("Goles anotados de local")
      .ylab("Número de partidos")
      .main("Cantidad de goles y partidos de Ecuador como local"))
    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\1.EcuadorLocal.png"), histograma.build, 1000)
  }

  /////////////////////GRAFICA 2 HISTOGRAMA//////////////
  def golesEcuadorVisitante(data: List[Map[String, String]]): List[Int] = {
    val visitanteEcuador = data.filter { matchData =>
      matchData("away_team_name") == "Ecuador"
    }
    val partidosVisitanteEcuador = visitanteEcuador.groupBy(_("matches_match_id")).map(_._2.head)
    val golesPorPartido = partidosVisitanteEcuador.map(row => row("matches_away_team_score").toInt)
    golesPorPartido.toList
  }

  def chartingEcuadorVisitante(data: List[Int]): Unit = {
    val histograma = xyplot(HistogramData(data.map(_.toDouble), 60) -> bar())(par
      .xlab("Goles anotados de visitante")
      .ylab("Número de partidos")
      .main("Cantidad de goles y partidos de Ecuador como visitante"))
    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\2.EcuadorVisitante.png"), histograma.build, 1000)
  }

  /////////////////////GRAFICA 3 SCATTER PLOT//////////////
  def datosGraficaAnioGoles(data: List[Map[String, String]]): List[(Int, Int)] = {
    val dataAñoGoles = data
      .map(row => (
        row("tournaments_year").toInt,
        row("matches_match_id"),
        row("matches_home_team_score").toInt,
        row("matches_away_team_score").toInt))
      .distinct.map(t4 => (t4._1, t4._3 + t4._4))
      .groupBy(_._1)
      .map(t2 => (t2._1, t2._2.map(_._2).sum) ).toList
      .sortBy(_._1)
    dataAñoGoles
  }
  def chartingAnioGoles(data: List[(Int, Int)]): Unit = {
    val listY: List[Double] = data.map(_._2.toDouble)
    val listX: List[Double] = data.map(_._1.toDouble)

    val agrupados = listX.zip(listY)
    val grafica = xyplot(agrupados -> point())(
      par
        .xlab("Año")
        .ylab("Total Goles")
        .main("TOTAL DE GOLES EN LOS AÑOS")
    )
    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\3.GolesAnio.png"), grafica.build)
  }


  /////////////////////GRAFICA 4 DENSITY PLOT//////////////
  def datosGraficaDefensasAñoDatos2(data: List[Map[String, String]], data2: List[Map[String, String]]): List[(Int, Int)] = {
    val defensas = data
      .map(row => (
        row("squads_tournament_id"),
        row("squads_position_name"),
        row("players_defender").toInt
      ))
      .filter(_._2.toLowerCase == "defender")
      .map(t4 => (t4._1, t4._3))
      .groupBy(_._1)
      .map(t2 => (t2._1, t2._2.map(_._2).sum))
      .toList
      .sortBy(_._1)
    val dataAños = data2
      .map(row => (
        row("matches_tournament_id"),
        row("tournaments_year").toInt,
      ))
      .distinct
      .groupBy(_._1)
      .map(t2 => (t2._1, t2._2.head._2))
      .toList.sortBy(_._1)
    val dataAñoDefensas = dataAños.flatMap {
      case (id, year) => defensas.find(_._1 == id).map {
        case (_, defensas) => (year, defensas)
      }
    }
    dataAñoDefensas
  }
  def chartingDefensores(data: List[(Int, Int)]): Unit = {
    val listY: List[Double] = data.map(_._2.toDouble)
    val listX: List[Double] = data.map(_._1.toDouble)
    val densityPlot = {
      val density1 = xyplot(
        density(listY.toIndexedSeq) -> line(stroke = StrokeConf(0.5.fts))
      )(par.withXLab("Cantidad de Defensores").withYLab("dens.").withMain("Distribución de Densidad de defensores"))
      density1
    }
    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\4.defensas.png"), densityPlot.build)
  }

  /////////////////////GRAFICA 5 BARPLOT VERTICAL//////////////
  def golesPorFaseDeGrupo(data: List[Map[String, String]]): List[(String, Int)] = {
    val partidosFaseGrupo = data.filter(_("matches_stage_name") == "group stage")
    val partidosPorAño = partidosFaseGrupo.groupBy(_("tournaments_year"))
    val totalGolesPorAño = partidosPorAño.map { case (año, partidos) =>
      val golesUnicos = partidos.groupBy(_("matches_match_id")).values.map { partido =>
        partido.head("matches_home_team_score").toInt + partido.head("matches_away_team_score").toInt
      }
      (año, golesUnicos.sum)
    }
    val listaGolesPorAñoOrdenada = totalGolesPorAño.toList.sortBy(_._1)

    listaGolesPorAñoOrdenada
  }
  def chartingFaseDeGrupos(data: List[(String, Int)]): Unit = {
    val labels = data.map(_._1)
    val values = data.map(_._2.toDouble)
    val bar1Data = labels.zip(values)
    val bar1 = barplotVertical(Series(bar1Data: _*))(par
      .withXLab("Goles totales en fase de grupos")
      .withYLab("Año del torneo")
      .withMain("Goles totales de fase de grupos por año"))
    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\5.fase.png"),
      group(bar1, TableLayout(1)).build, width = 5000)
  }


  /////////////////////GRAFICA 6 BARPLOT DOBLE//////////////
  def datosGraficaCiudadEstadio(data: List[Map[String, String]]): List[(String, Int)] = {
    val estadiosPorCiudad = data
      .map(row => (
        row("stadiums_country_name"),
        row("stadiums_stadium_name")
      ))
      .distinct
      .groupBy(_._1)
      .map { case (ciudad, estadios) =>
        (ciudad, estadios.length)
      }
      .toList
      .sortBy(_._1)
    estadiosPorCiudad
  }

  def chartingEstadios(data: List[(String, Int)]): Unit = {
    val labels = data.map(_._1)
    val values = data.map(_._2.toDouble)

    val bar1Data = labels.take(10).zip(values.take(10))
    val bar2Data = labels.drop(10).zip(values.drop(10))

    val plotSettings = par
      .xlab("Cantidad")
      .ylab("Numero del pais")

    val bar1 = barplotVertical(Series(bar1Data: _*))(plotSettings.main("10 PRIMEROS PAICES Y SU TOTAL DE ESTADIOS"))
    val bar2 = barplotVertical(Series(bar2Data: _*))(plotSettings.main("10 ULTIMOS PAICES Y SU TOTAL DE ESTADIOS"))

    pngToFile(new File("C:\\Users\\JOSE\\Desktop\\PROGR FUNCIONAL\\archivos\\6.estadiosCiudad.png"),
      group(bar1, bar2, TableLayout(2)).build, width = 5000)
  }
}