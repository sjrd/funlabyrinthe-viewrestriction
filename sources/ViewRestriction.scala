package user.sjrd.viewrestriction

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object ViewRestriction extends Module:
  override protected def preInitialize()(using Universe): Unit =
    val viewRestrictionRadius = newAttribute[Int](40)
  end preInitialize

  override protected def createComponents()(using Universe): Unit =
    val viewRestrictionPlugin = new ViewRestrictionPlugin
  end createComponents

  def viewRestrictionRadius(using Universe): Attribute[Int] =
    myAttributeByID("viewRestrictionRadius")

  def viewRestrictionPlugin(using Universe): ViewRestrictionPlugin =
    myComponentByID("viewRestrictionPlugin")
end ViewRestriction

export ViewRestriction.*

class ViewRestrictionPlugin(using ComponentInit) extends PlayerPlugin:
  zindex = 512

  @transient
  private var maskCacheParams: Option[(Int, Double, Double)] = None
  @transient
  private var maskCache: Option[Canvas] = None
  
  override def drawView(corePlayer: CorePlayer, context: DrawContext): Unit =
    import context.*

    val player = corePlayer.reified[Player]
    for playerPos <- player.position do
      val restrictionRadius = player.attributes(viewRestrictionRadius)
      val mask = updateCacheAndGet(restrictionRadius, width, height)

      // Build center
      val map = playerPos.map
      val pos = playerPos.pos
      val centerX = (Math.floorMod(pos.x, map.zoneWidth) + 1) * map.SquareWidth + (map.SquareWidth / 2)
      val centerY = (Math.floorMod(pos.y, map.zoneHeight) + 1) * map.SquareHeight + (map.SquareHeight / 2)

      // Build topLeft
      val left = centerX - width
      val top = centerY - height

      // Draw mask bitmap
      gc.drawImage(mask, left, top)
    end for
  end drawView

  private def updateCacheAndGet(restrictionRadius: Int, viewWidth: Double, viewHeight: Double): Canvas =
    val params = Some((restrictionRadius, viewWidth, viewHeight))
    if maskCacheParams != params then
      val width = viewWidth * 2
      val height = viewHeight * 2
      maskCache = Some(universe.graphicsSystem.createCanvas(width, height))
      val gc = maskCache.get.getGraphicsContext2D()

      gc.fill = Color.Black
      gc.fillRect(0, 0, width, height)

      val maxRadius = restrictionRadius
      val maxRadiusSquare = maxRadius * maxRadius

      val centerX = viewWidth.toInt
      val centerY = viewWidth.toInt

      for
        x <- (centerX - maxRadius) to (centerX + maxRadius)
        y <- (centerY - maxRadius) to (centerY + maxRadius)
      do
        val diffX = x - centerX
        val diffY = y - centerY
        val distSquare = diffX*diffX + diffY*diffY
        if distSquare < maxRadiusSquare then
          gc.clearRect(x, y, 1, 1)
          gc.fill = Color(0, 0, 0, distSquare.toDouble / maxRadiusSquare.toDouble)
          gc.fillRect(x, y, 1, 1)
      end for

      maskCacheParams = params
    end if

    maskCache.get
  end updateCacheAndGet
end ViewRestrictionPlugin