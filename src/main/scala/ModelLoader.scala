package modelloader

import scala.util.Using

import java.io.File
import java.nio.IntBuffer

import org.joml.*
import org.lwjgl.PointerBuffer
import org.lwjgl.assimp.*
import org.lwjgl.assimp.Assimp.*


def loadModel(file: File, flags: Int): Model =
  Using.resource(aiImportFile(file.getAbsolutePath(), flags)) { scene =>
    if scene == null || scene.mRootNode() == null then
      throw RuntimeException("Could not load model: " + aiGetErrorString())
    processNode(scene.mRootNode(), scene)
  }

def processNode(node: AINode, scene: AIScene): Model =
  val thisNodeModel: Model =
    if node.mMeshes != null then processNodeMeshes(scene, node)
    else Model.empty

  val childrenModel: Model =
    if node.mChildren != null then
      (for
        i <- 0 until node.mNumChildren
        child = AINode.create(node.mChildren.get(i))
      yield
        processNode(child, scene)).combine
    else Model.empty

  thisNodeModel + childrenModel
end processNode

def processNodeMeshes(scene: AIScene, node: AINode): Model =
  (for
    i <- 0 until node.mMeshes.capacity
    mesh = AIMesh.create(scene.mMeshes.get(node.mMeshes.get(i)))
  yield
    processMesh(scene, mesh)).combine

def processMesh(scene: AIScene, mesh: AIMesh): Model =
  def processPositions(mesh: AIMesh): List[Vector3f] =
    val vertices = mesh.mVertices
    for i <- (0 until vertices.capacity).toList yield
      val position = vertices.get(i)
      Vector3f(position.x, position.y, position.z)

  def processTexCoords(mesh: AIMesh): List[Vector2f] =
    val aiTexCoords = mesh.mTextureCoords(0)
    if aiTexCoords == null then Nil else
      for i <- (0 until aiTexCoords.capacity).toList yield
        val coords = aiTexCoords.get(i)
        Vector2f(coords.x, coords.y)

  def processIndices(mesh: AIMesh): List[Int] =
    for
      i <- (0 until mesh.mNumFaces).toList
      face = mesh.mFaces.get(i)
      pIndices = face.mIndices
      j <- 0 until face.mNumIndices
    yield pIndices.get(j)

  Model(processPositions(mesh), processTexCoords(mesh), processIndices(mesh))
end processMesh

case class Model(positions: List[Vector3f], texCoords: List[Vector2f], indices: List[Int]):
  def +(another: Model) = Model(positions ++ another.positions,
    texCoords ++ another.texCoords, indices ++ another.indices)

object Model:
  def empty = Model(Nil, Nil, Nil)

extension (ms: Iterable[Model]) def combine: Model = ms.foldLeft(Model.empty)(_+_)
