package render

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import THREE.BufferGeometry

// Additional Three.js classes not in the threesjs library

@js.native @JSImport("three", "Color")
class ThreeColor(value: js.Any = js.undefined) extends js.Object

@js.native @JSImport("three", "PlaneGeometry")
class PlaneGeometry(
  width: Double = 1,
  height: Double = 1,
  widthSegments: Int = 1,
  heightSegments: Int = 1
) extends BufferGeometry

@js.native @JSImport("three", "AmbientLight")
class AmbientLight(color: js.Any = js.undefined, intensity: Double = 1.0) extends THREE.Object3D

@js.native @JSImport("three", "DirectionalLight")
class DirectionalLight(color: js.Any = js.undefined, intensity: Double = 1.0) extends THREE.Object3D

@js.native @JSImport("three", "CylinderGeometry")
class CylinderGeometry(
  radiusTop:      Double = 1.0,
  radiusBottom:   Double = 1.0,
  height:         Double = 1.0,
  radialSegments: Int    = 32
) extends THREE.BufferGeometry

@js.native @JSImport("three", "Group")
class ThreeGroup() extends THREE.Object3D
