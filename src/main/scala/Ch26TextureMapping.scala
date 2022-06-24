package Ch26TextureMapping

import reflect.Selectable.reflectiveSelectable

import scala.util.Using
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWVulkan.*

import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.EXTDebugUtils.*
import org.lwjgl.vulkan.KHRSurface.*
import org.lwjgl.vulkan.KHRSwapchain.*

import org.lwjgl.system.*
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.PointerBuffer
import org.lwjgl.util.shaderc.Shaderc.*
import org.lwjgl.stb.STBImage.*

import org.joml.{ Vector2f, Vector3f, Matrix4f }

import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.Buffer
import java.nio.ByteBuffer
import javax.management.Query
import org.joml.sampling.UniformSampling


var window = -1l  // TODO replace -1 with VK_NULL_HANDLE everywhere
var instance: VkInstance = null
var physicalDevice: VkPhysicalDevice = null
var device: VkDevice = null
var surface: Long = -1l
var graphicsQueue: VkQueue = null
var presentQueue: VkQueue = null
var debugMessenger: Long = -1l

var swapChain: Long = VK_NULL_HANDLE
var swapChainImages: List[Long] = List.empty
var swapChainImageFormat: Int = -1
var swapChainExtent: VkExtent2D = null
var swapChainImageViews: List[Long] = List.empty
var swapChainFramebuffers: List[Long] = List.empty

var renderPass: Long = -1
var descriptorPool: Long = VK_NULL_HANDLE
var descriptorSetLayout: Long = VK_NULL_HANDLE
var descriptorSets: List[Long] = List.empty
var pipelineLayout: Long = -1
var graphicsPipeline: Long = -1

var commandPool: Long = -1
var commandBuffers: List[VkCommandBuffer] = List.empty

var imageAvailableSemaphores: List[Long] = List.empty
var renderFinishedSemaphores: List[Long] = List.empty
var inFlightFences: List[Long] = List.empty

var textureImage = VK_NULL_HANDLE
var textureImageMemory = VK_NULL_HANDLE
var textureImageView = VK_NULL_HANDLE
var textureSampler = VK_NULL_HANDLE

var vertexBuffer: Long = VK_NULL_HANDLE
var vertexBufferMemory: Long = VK_NULL_HANDLE
var indexBuffer: Long = VK_NULL_HANDLE
var indexBufferMemory: Long = VK_NULL_HANDLE
var uniformBuffers: List[Long] = List.empty
var uniformBuffersMemory: List[Long] = List.empty

val enableValidationLayers = true
val maxFramesInFlight = 2

val validationLayers = List("VK_LAYER_KHRONOS_validation")
val deviceExtensions = List(VK_KHR_SWAPCHAIN_EXTENSION_NAME)

case class Vertex(pos: Vector2f, color: Vector3f, texCoords: Vector2f)
object Vertex:
  val size = (2 + 3 + 2) * java.lang.Float.BYTES
  val offsetPos = 0
  val offsetColor = 2 * java.lang.Float.BYTES
  val offsetTexCoords = (2 + 3) * java.lang.Float.BYTES

  def getBindingDescription()(using stack: MemoryStack): VkVertexInputBindingDescription.Buffer =
    VkVertexInputBindingDescription.callocStack(1, stack)
      .binding(0)
      .stride(size)
      .inputRate(VK_VERTEX_INPUT_RATE_VERTEX)

  def getAttributeDescriptions()(using stack: MemoryStack): VkVertexInputAttributeDescription.Buffer =
      val attributeDescriptions = VkVertexInputAttributeDescription.callocStack(3, stack)

      // Position
      attributeDescriptions.get(0)
        .binding(0)
        .location(0)
        .format(VK_FORMAT_R32G32_SFLOAT)
        .offset(offsetPos)

      // Color
      attributeDescriptions.get(1)
        .binding(0)
        .location(1)
        .format(VK_FORMAT_R32G32B32_SFLOAT)
        .offset(offsetColor)

      // Texture Coordinates
      attributeDescriptions.get(2)
        .binding(0)
        .location(2)
        .format(VK_FORMAT_R32G32_SFLOAT)
        .offset(offsetTexCoords)

      attributeDescriptions.rewind()
  end getAttributeDescriptions
end Vertex

val vertices = List(
  Vertex(Vector2f(-0.5f, -0.5f), Vector3f(1.0f, 0.0f, 0.0f), Vector2f(1.0f, 0.0f)),
  Vertex(Vector2f(0.5f, -0.5f), Vector3f(0.0f, 1.0f, 0.0f), Vector2f(0.0f, 0.0f)),
  Vertex(Vector2f(0.5f, 0.5f), Vector3f(0.0f, 0.0f, 1.0f), Vector2f(0.0f, 1.0f)),
  Vertex(Vector2f(-0.5f, 0.5f), Vector3f(1.0f, 1.0f, 1.0f), Vector2f(1.0f, 1.0f)),
)
val indices = List[Short](0, 1, 2, 2, 3, 0)

case class UniformBufferObject(model: Matrix4f,
  view: Matrix4f, proj: Matrix4f)
object UniformBufferObject:
  val size = 3 * 16 * java.lang.Float.BYTES


@main def Ch26TextureMapping =
  initWindow()
  initVulkan()
  loop()
  cleanup()

def initWindow() =
  if !glfwInit() then throw RuntimeException("Cannot init GLFW")
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
  window = glfwCreateWindow(800, 600, this.getClass.getSimpleName, 0, 0)
  if window == 0 then throw RuntimeException("Cannot create window")


enum FamilyType { case graphicsFamily, presentFamily }
def findQueueFamiliesIds(device: VkPhysicalDevice)(using stack: MemoryStack): Map[FamilyType, List[Int]] =
  val queueFamiliesProps = querySeq(vkGetPhysicalDeviceQueueFamilyProperties(device, _, _: VkQueueFamilyProperties.Buffer))

  def familiesSupported(props: VkQueueFamilyProperties, qid: Int): List[FamilyType] =
    def presentation(): Option[FamilyType] =
      val presentSupport = query(vkGetPhysicalDeviceSurfaceSupportKHR(device, qid, surface, _: IntBuffer))
      if presentSupport == 1 then Some(FamilyType.presentFamily) else None

    def graphics(): Option[FamilyType] =
      if (props.queueFlags & VK_QUEUE_GRAPHICS_BIT) != 0 then Some(FamilyType.graphicsFamily) else None

    Nil ++ presentation() ++ graphics()
  end familiesSupported

  ( for
      (props, qid) <- queueFamiliesProps.zipWithIndex
      familyTpe <- familiesSupported(props, qid)
    yield
      familyTpe -> qid
  ).groupMap(_._1)(_._2)  // familyTpe -> List[queueId]
end findQueueFamiliesIds

def initVulkan() = Using.resource(stackPush()) { stack =>
  given MemoryStack = stack

  def checkValidationLayerSupport() =
    val availableLayers = querySeq(vkEnumerateInstanceLayerProperties).map(_.layerNameString).toSet
    val unavailableLayers = validationLayers.filterNot(availableLayers)
    if unavailableLayers.nonEmpty then
      throw RuntimeException(s"The following layers are not supported: ${unavailableLayers.mkString(", ")}")
  end checkValidationLayerSupport

  def getRequiredExtensions() =
    val glfwExtensions: PointerBuffer = glfwGetRequiredInstanceExtensions()
    if enableValidationLayers then
      val extensions: PointerBuffer = stack.mallocPointer(glfwExtensions.capacity() + 1)
      (extensions: CustomBuffer[PointerBuffer]).put(glfwExtensions)
      extensions.put(stack.UTF8(VK_EXT_DEBUG_UTILS_EXTENSION_NAME))
      extensions.rewind()
    else
      glfwExtensions
  end getRequiredExtensions

  lazy val debugMessengerCreateInfo =
    val createInfo = VkDebugUtilsMessengerCreateInfoEXT.callocStack(stack)
    createInfo.sType(VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    createInfo.messageSeverity(VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
    createInfo.messageType(VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)
    createInfo.pfnUserCallback((messageSeverity, messageType, pCallbackData, pUserData) => {
      val callbackData = VkDebugUtilsMessengerCallbackDataEXT.create(pCallbackData)
      println("Validation layer: " + callbackData.pMessageString())
      VK_FALSE
    })
    createInfo

  def createInstance() =
    val appInfo = VkApplicationInfo.calloc(stack)
    appInfo.sType(VK_STRUCTURE_TYPE_APPLICATION_INFO)
    appInfo.pApplicationName(stack.UTF8("Hello Triangle"))
    appInfo.applicationVersion(VK_MAKE_VERSION(1, 0, 0))
    appInfo.pEngineName(stack.UTF8("No Engine"))
    appInfo.engineVersion(VK_MAKE_VERSION(1, 0, 0))
    appInfo.apiVersion(VK_API_VERSION_1_0)

    val createInfo = VkInstanceCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
      .pApplicationInfo(appInfo)
      .ppEnabledExtensionNames(getRequiredExtensions())
    if enableValidationLayers then
      checkValidationLayerSupport()
      createInfo.ppEnabledLayerNames(asPointerBuffer(validationLayers, stack))
      createInfo.pNext(debugMessengerCreateInfo)

    instance = VkInstance(create(vkCreateInstance(createInfo, null, _)), createInfo)
  end createInstance

  def pickPhysicalDevice() =
    def isDeviceSuitable(device: VkPhysicalDevice): Boolean =
      def checkQueueSupport() =
        val fams = findQueueFamiliesIds(device)
        fams.contains(FamilyType.graphicsFamily) &&
          fams.contains(FamilyType.presentFamily)

      def checkDeviceExtensionSupport(): Boolean =
        val availableExtensions = querySeq(vkEnumerateDeviceExtensionProperties(device, null: CharSequence, _, _: VkExtensionProperties.Buffer))
          .toSet.map(_.extensionNameString)
        deviceExtensions.forall(availableExtensions.contains)

      def checkSwapChainSupport(): Boolean =
        val swapChainSupport = SwapChainLifecycle.querySwapChainSupport(device)
        swapChainSupport.formats.nonEmpty && swapChainSupport.presentModes.nonEmpty

      def checkAnisotropySupport(): Boolean =
        queryStruct(vkGetPhysicalDeviceFeatures(device, _)).samplerAnisotropy

      checkQueueSupport() && checkDeviceExtensionSupport() &&
      checkSwapChainSupport() && checkAnisotropySupport()
    end isDeviceSuitable

    val devices = querySeq(vkEnumeratePhysicalDevices(instance, _, _: PointerBuffer)).map(VkPhysicalDevice(_, instance))
    physicalDevice = devices.find(isDeviceSuitable).get
  end pickPhysicalDevice

  def createLogicalDevice() =
    val queueFamiliesMap = findQueueFamiliesIds(physicalDevice)
    val graphicsFamilyId = queueFamiliesMap(FamilyType.graphicsFamily).head
    val presentFamilyId = queueFamiliesMap(FamilyType.presentFamily).head
    val queueFamiliesIds = List(graphicsFamilyId, presentFamilyId).distinct

    val queueCreateInfos = VkDeviceQueueCreateInfo.calloc(queueFamiliesIds.length, stack)
    for
      i <- 0 until queueFamiliesIds.length
      queueCreateInfo = queueCreateInfos.get(i)
      qid = queueFamiliesIds(i)
    do
      queueCreateInfos.sType(VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
      queueCreateInfos.queueFamilyIndex(qid)
      queueCreateInfos.pQueuePriorities(stack.floats(1.0f))

    val createInfo = VkDeviceCreateInfo.calloc(stack)
    createInfo.sType(VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
    createInfo.pQueueCreateInfos(queueCreateInfos)
    createInfo.pEnabledFeatures(VkPhysicalDeviceFeatures.calloc(stack)
      .samplerAnisotropy(true))
    createInfo.ppEnabledExtensionNames(asPointerBuffer(deviceExtensions, stack))
    if enableValidationLayers then
      createInfo.ppEnabledLayerNames(asPointerBuffer(validationLayers, stack))

    device = VkDevice(create(vkCreateDevice(physicalDevice, createInfo, null, _)), physicalDevice, createInfo)
    graphicsQueue = VkQueue(query(vkGetDeviceQueue(device, graphicsFamilyId, 0, _: PointerBuffer)), device)
    presentQueue = VkQueue(query(vkGetDeviceQueue(device, presentFamilyId, 0, _: PointerBuffer)), device)
  end createLogicalDevice

  def createCommandPool() =
    val queueFamilyIndices = findQueueFamiliesIds(physicalDevice)

    val poolInfo = VkCommandPoolCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
      .queueFamilyIndex(queueFamilyIndices(FamilyType.graphicsFamily).head)
      .flags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
    commandPool = create(vkCreateCommandPool(device, poolInfo, null, _: LongBuffer))
  end createCommandPool

  def createCommandBuffers() =
    val allocInfo = VkCommandBufferAllocateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
      .commandPool(commandPool)
      .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      .commandBufferCount(maxFramesInFlight)

    commandBuffers = createMany(vkAllocateCommandBuffers(device, allocInfo, _: PointerBuffer),
      maxFramesInFlight).map(VkCommandBuffer(_, device))
  end createCommandBuffers

  def createSyncObjects() =
    val semaphoreInfo = VkSemaphoreCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
    val fenceInfo = VkFenceCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_FENCE_CREATE_INFO)
      .flags(VK_FENCE_CREATE_SIGNALED_BIT)

    imageAvailableSemaphores = createManyLooped(vkCreateSemaphore(device, semaphoreInfo, null, _: LongBuffer), maxFramesInFlight)
    renderFinishedSemaphores = createManyLooped(vkCreateSemaphore(device, semaphoreInfo, null, _: LongBuffer), maxFramesInFlight)
    inFlightFences = createManyLooped(vkCreateFence(device, fenceInfo, null, _: LongBuffer), maxFramesInFlight)
  end createSyncObjects

  def findMemoryType(typeFilter: Int, properties: Int): Int =
    queryStruct(vkGetPhysicalDeviceMemoryProperties(physicalDevice, _))
      .memoryTypes.toList.zipWithIndex.collectFirst {
        case (memTpe, i) if (typeFilter & (1 << i)) != 0 &&
          (memTpe.propertyFlags & properties) == properties => i }
      .getOrElse(throw RuntimeException("Failed to find suitable memory type"))

  def createBuffer(size: Long, usage: Int, properties: Int,
      bufferBinder: Long => Unit = _ => (), bufferMemoryBinder: Long => Unit = _ => ()): (Long, Long) =  // Buffer, BufferMemory
    val bufferInfo = VkBufferCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
      .size(size)
      .usage(usage)
      .sharingMode(VK_SHARING_MODE_EXCLUSIVE)

    val buffer = create(vkCreateBuffer(device, bufferInfo, null, _: LongBuffer))

    val memRequirements = queryStruct(vkGetBufferMemoryRequirements(device, buffer, _))

    val allocInfo = VkMemoryAllocateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
      .allocationSize(memRequirements.size)
      .memoryTypeIndex(findMemoryType(memRequirements.memoryTypeBits, properties))
    val bufferMemory = create(vkAllocateMemory(device, allocInfo, null, _: LongBuffer))
    vkBindBufferMemory(device, buffer, bufferMemory, 0)

    bufferBinder(buffer)
    bufferMemoryBinder(bufferMemory)
    (buffer, bufferMemory)
  end createBuffer

  def copyBuffer(src: Long, dest: Long, size: Int): Unit =
    val commandBufferPtr = create(vkAllocateCommandBuffers(device,
      VkCommandBufferAllocateInfo.calloc(stack)
        .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
        .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
        .commandPool(commandPool)
        .commandBufferCount(1)
      , _: PointerBuffer))
    val commandBuffer = VkCommandBuffer(commandBufferPtr, device)

    vkBeginCommandBuffer(commandBuffer, VkCommandBufferBeginInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
      .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT))
    vkCmdCopyBuffer(commandBuffer, src, dest, VkBufferCopy.calloc(1, stack).size(size))
    vkEndCommandBuffer(commandBuffer)

    vkQueueSubmit(graphicsQueue, VkSubmitInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_SUBMIT_INFO)
      .pCommandBuffers(stack.pointers(commandBuffer)), VK_NULL_HANDLE)
    vkQueueWaitIdle(graphicsQueue)

    vkFreeCommandBuffers(device, commandPool, commandBuffer)
  end copyBuffer

  def createVertexBuffer() =
    val bufferSize = Vertex.size * vertices.length

    val (stagingBuffer, stagingBufferMemory) = createBuffer(
      bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    createBuffer(
      bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
      vertexBuffer = _, vertexBufferMemory = _)

    val data = queryStruct(vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, _: PointerBuffer))
      .getByteBuffer(0, bufferSize)
    for v <- vertices do data
      .putFloat(v.pos.x)
      .putFloat(v.pos.y)
      .putFloat(v.color.x)
      .putFloat(v.color.y)
      .putFloat(v.color.z)
      .putFloat(v.texCoords.x)
      .putFloat(v.texCoords.y)
    vkUnmapMemory(device, stagingBufferMemory)

    copyBuffer(stagingBuffer, vertexBuffer, bufferSize)

    vkDestroyBuffer(device, stagingBuffer, null)
    vkFreeMemory(device, stagingBufferMemory, null)
  end createVertexBuffer

  def createIndexBuffer() =
    val bufferSize = java.lang.Short.BYTES * indices.size

    val (stagingBuffer, stagingBufferMemory) = createBuffer(bufferSize,
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    createBuffer(bufferSize,
      VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, indexBuffer = _, indexBufferMemory = _)

    val data = queryStruct(vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, _: PointerBuffer))
      .getByteBuffer(0, bufferSize)
    indices.foreach(data.putShort)
    vkUnmapMemory(device, stagingBufferMemory)

    copyBuffer(stagingBuffer, indexBuffer, bufferSize)

    vkDestroyBuffer(device, stagingBuffer, null)
    vkFreeMemory(device, stagingBufferMemory, null)
  end createIndexBuffer

  def createUniformBuffers() =
    val bufferSize = UniformBufferObject.size
    val buffersAndMemories =
      List.fill(maxFramesInFlight)(createBuffer(bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

    uniformBuffers = buffersAndMemories.map(_._1)
    uniformBuffersMemory = buffersAndMemories.map(_._2)
  end createUniformBuffers

  def createDescriptorSetLayout() =
    val bindings = VkDescriptorSetLayoutBinding.calloc(2, stack)

    bindings.get(0) // UBO layout binding
      .binding(0)
      .descriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
      .descriptorCount(1)
      .stageFlags(VK_SHADER_STAGE_VERTEX_BIT)
      .pImmutableSamplers(null) // Optional

    bindings.get(1)  // Texture sampler binding
      .binding(1)
      .descriptorCount(1)
      .descriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
      .pImmutableSamplers(null)
      .stageFlags(VK_SHADER_STAGE_FRAGMENT_BIT)

    descriptorSetLayout = create(vkCreateDescriptorSetLayout(device, VkDescriptorSetLayoutCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
      .pBindings(bindings)
      , null, _: LongBuffer))
  end createDescriptorSetLayout

  def createDescriptorPool() =
    val poolSizes = VkDescriptorPoolSize.callocStack(2, stack)
    poolSizes.get(0)
      .`type`(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
      .descriptorCount(swapChainImages.size)
    poolSizes.get(1)
      .`type`(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
      .descriptorCount(swapChainImages.size)

    descriptorPool = create(vkCreateDescriptorPool(device, VkDescriptorPoolCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
      .pPoolSizes(poolSizes)
      .maxSets(swapChainImages.size)
      , null, _: LongBuffer))
  end createDescriptorPool

  def createDescriptorSets() =
    val layouts = stack.mallocLong(swapChainImages.size)
    for i <- 0 until layouts.capacity do layouts.put(i, descriptorSetLayout)

    val allocInfo = VkDescriptorSetAllocateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
      .descriptorPool(descriptorPool)
      .pSetLayouts(layouts)
    descriptorSets = createMany(vkAllocateDescriptorSets(device, allocInfo, _: LongBuffer), swapChainImages.size)

    val bufferInfo = VkDescriptorBufferInfo.callocStack(1, stack)
      .offset(0).range(UniformBufferObject.size)

    val descriptorWrites = VkWriteDescriptorSet.callocStack(2, stack)
    val uboDescriptorWrite = descriptorWrites.get(0)
      .sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
      .descriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
      .dstBinding(0)
      .pBufferInfo(bufferInfo)
      .dstArrayElement(0)
      .descriptorCount(1)
    val samplerDescriptorWrite = descriptorWrites.get(1)
      .sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
      .dstBinding(1)
      .dstArrayElement(0)
      .descriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
      .descriptorCount(1)
      .pImageInfo(VkDescriptorImageInfo.callocStack(1, stack)
        .imageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        .imageView(textureImageView)
        .sampler(textureSampler))

    for (descriptorSet, uniformBuffer) <- descriptorSets.zip(uniformBuffers) do
      bufferInfo.buffer(uniformBuffer)
      uboDescriptorWrite.dstSet(descriptorSet)
      samplerDescriptorWrite.dstSet(descriptorSet)
      vkUpdateDescriptorSets(device, descriptorWrites, null)
  end createDescriptorSets

  def withMemoryMap[T](memory: Long, size: Int)(f: ByteBuffer => T): T =  // TODO replace also other occurrences
    val data = queryStruct(vkMapMemory(device, memory, 0, size, 0, _: PointerBuffer)).getByteBuffer(0, size)
    val res = f(data)  // TODO: perfect candidate for capture calculus
    vkUnmapMemory(device, memory)
    res

  def createImage(width: Int, height: Int, format: Int, tiling: Int, usage: Int, memProperties: Int,
    imageBinder: Long => Unit = _ => (), memoryBinder: Long => Unit = _ => ()): (Long, Long) =  // Image, Memory
    val imageInfo = VkImageCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO)
      .imageType(VK_IMAGE_TYPE_2D)
      .mipLevels(1)
      .arrayLayers(1)
      .format(format)
      .tiling(tiling)
      .initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
      .usage(usage)
      .samples(VK_SAMPLE_COUNT_1_BIT)
      .sharingMode(VK_SHARING_MODE_EXCLUSIVE)
    imageInfo
      .extent
        .width(width)
        .height(height)
        .depth(1)
    val textureImage = create(vkCreateImage(device, imageInfo, null, _: LongBuffer))

    val memRequirements = queryStruct(vkGetImageMemoryRequirements(device, textureImage, _))
    val textureImageMemory = create(vkAllocateMemory(device, VkMemoryAllocateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
      .allocationSize(memRequirements.size)
      .memoryTypeIndex(findMemoryType(memRequirements.memoryTypeBits, memProperties))
      , null, _: LongBuffer))
    vkBindImageMemory(device, textureImage, textureImageMemory, 0);

    imageBinder(textureImage)
    memoryBinder(textureImageMemory)
    (textureImage, textureImageMemory)
  end createImage

  def withSingleTimeCommandBuffer[T](f: VkCommandBuffer => T): T =  // TODO replace also other occurrences; also: can we have a global initialization-time buffer?
    val commandBuffer = VkCommandBuffer(
      create(vkAllocateCommandBuffers(device, VkCommandBufferAllocateInfo.callocStack(stack)
        .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
        .level(VK_COMMAND_BUFFER_LEVEL_PRIMARY)
        .commandPool(commandPool)
        .commandBufferCount(1)
        , _: PointerBuffer)), device)

    vkBeginCommandBuffer(commandBuffer, VkCommandBufferBeginInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
      .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT))

    val res = f(commandBuffer)  // TODO: perfect candidate for capture calculus

    vkEndCommandBuffer(commandBuffer);
    vkQueueSubmit(graphicsQueue, VkSubmitInfo.callocStack(1, stack)
      .sType(VK_STRUCTURE_TYPE_SUBMIT_INFO)
      .pCommandBuffers(stack.pointers(commandBuffer))
      , VK_NULL_HANDLE)
    vkQueueWaitIdle(graphicsQueue)
    vkFreeCommandBuffers(device, commandPool, commandBuffer)

    res
  end withSingleTimeCommandBuffer

  def transitionImageLayout(image: Long, format: Int, oldLayout: Int, newLayout: Int): Unit =
    val barrier = VkImageMemoryBarrier.callocStack(1, stack)
      .sType(VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
      .oldLayout(oldLayout)
      .newLayout(newLayout)
      .srcQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
      .dstQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED)
      .image(image)
    barrier
      .subresourceRange
        .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
        .baseMipLevel(0)
        .levelCount(1)
        .baseArrayLayer(0)
        .layerCount(1)

    var sourceStage = 0
    var destinationStage = 0
    if  oldLayout == VK_IMAGE_LAYOUT_UNDEFINED &&
        newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    then
      barrier
        .srcAccessMask(0)
        .dstAccessMask(VK_ACCESS_TRANSFER_WRITE_BIT)
      sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
      destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT
    else if
        oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
        newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    then
      barrier
        .srcAccessMask(VK_ACCESS_TRANSFER_WRITE_BIT)
        .dstAccessMask(VK_ACCESS_SHADER_READ_BIT)
      sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT
      destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    else throw IllegalArgumentException("Unsupported layout transition")

    withSingleTimeCommandBuffer { commandBuffer =>
      vkCmdPipelineBarrier(commandBuffer, sourceStage, destinationStage,
        0, null, null, barrier)
    }
  end transitionImageLayout

  def copyBufferToImage(buffer: Long, image: Long, width: Int, height: Int): Unit =
    withSingleTimeCommandBuffer { commandBuffer =>
      val region = VkBufferImageCopy.callocStack(1, stack)
        .bufferOffset(0)
        .bufferRowLength(0)
        .bufferImageHeight(0)
        .imageExtent(VkExtent3D.callocStack(stack).set(width, height, 1))
      region
        .imageSubresource
          .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
          .mipLevel(0)
          .baseArrayLayer(0)
          .layerCount(1)
      region.imageOffset.set(0, 0, 0)
      vkCmdCopyBufferToImage(commandBuffer, buffer, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, region)
    }

  def createTextureImage() =
    // val filename = ClassLoader.getSystemClassLoader.getResource("textures/texture.jpg").toExternalForm  // TODO learn how to read filename from resource
    val filename = "src/main/resources/textures/cat.jpg"
    val List(pWidth, pHeight, pChannels) = List.fill(3)(stack.mallocInt(1))
    val pixels = stbi_load(filename, pWidth, pHeight, pChannels, STBI_rgb_alpha)
    val imageSize = pWidth.get(0) * pHeight.get(0) * 4

    if pixels == null then throw RuntimeException(s"Failed to load image $filename")

    val (stagingBuffer, stagingBufferMemory) = createBuffer(imageSize,
      VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

    withMemoryMap(stagingBufferMemory, imageSize) { data =>
      pixels.limit(imageSize)  // TODO do we need this? Comment it out?
      data.put(pixels)
      pixels.limit(pixels.capacity).rewind()
    }
    stbi_image_free(pixels)

    createImage(pWidth.get(0), pHeight.get(0),
      VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_TILING_OPTIMAL,
      VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
      VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, textureImage = _, textureImageMemory = _)
    transitionImageLayout(textureImage,
      VK_FORMAT_R8G8B8A8_SRGB,
      VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
    copyBufferToImage(stagingBuffer, textureImage, pWidth.get(0), pHeight.get(0))
    transitionImageLayout(textureImage,
      VK_FORMAT_R8G8B8A8_SRGB,
      VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)

    vkDestroyBuffer(device, stagingBuffer, null)
    vkFreeMemory(device, stagingBufferMemory, null)
  end createTextureImage

  def createTextureImageView() =
    textureImageView = createImageView(textureImage, VK_FORMAT_R8G8B8A8_SRGB)

  def createImageView(image: Long, format: Int) =  // TODO see if the swapchain image creation can also be abstracted with this
    val viewInfo = VkImageViewCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
      .image(image)
      .viewType(VK_IMAGE_VIEW_TYPE_2D)
      .format(format)
    viewInfo
      .subresourceRange
        .aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
        .baseMipLevel(0)
        .levelCount(1)
        .baseArrayLayer(0)
        .layerCount(1)
    create(vkCreateImageView(device, viewInfo, null, _: LongBuffer))

  def createTextureSampler() =
    textureSampler = create(vkCreateSampler(device, VkSamplerCreateInfo.callocStack(stack)
      .sType(VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
      .magFilter(VK_FILTER_LINEAR)
      .minFilter(VK_FILTER_LINEAR)
      .addressModeU(VK_SAMPLER_ADDRESS_MODE_REPEAT)
      .addressModeV(VK_SAMPLER_ADDRESS_MODE_REPEAT)
      .addressModeW(VK_SAMPLER_ADDRESS_MODE_REPEAT)
      .anisotropyEnable(true)
      .maxAnisotropy(16.0f)
      .borderColor(VK_BORDER_COLOR_INT_OPAQUE_BLACK)
      .unnormalizedCoordinates(false)
      .compareEnable(false)
      .compareOp(VK_COMPARE_OP_ALWAYS)
      .mipmapMode(VK_SAMPLER_MIPMAP_MODE_LINEAR)
      , null, _: LongBuffer))

  createInstance()
  if enableValidationLayers then
    debugMessenger = create(vkCreateDebugUtilsMessengerEXT(instance, debugMessengerCreateInfo, null, _: LongBuffer))
  surface = create(glfwCreateWindowSurface(instance, window, null, _: LongBuffer))
  pickPhysicalDevice()
  createLogicalDevice()
  createDescriptorSetLayout()
  SwapChainLifecycle.initializeSwapChain()

  createCommandPool()
  createCommandBuffers()
  createSyncObjects()

  createVertexBuffer()
  createIndexBuffer()
  createUniformBuffers()
  createTextureImage()
  createTextureImageView()
  createTextureSampler()
  createDescriptorPool()
  createDescriptorSets()
}

object SwapChainLifecycle:
  case class SwapChainSupportDetails(
    capabilities: VkSurfaceCapabilitiesKHR,
    formats: List[VkSurfaceFormatKHR],
    presentModes: List[Int],
  )

  def querySwapChainSupport(physicalDevice: VkPhysicalDevice)(using stack: MemoryStack): SwapChainSupportDetails =
    val capabilities = queryStruct(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface, _))
    val formats = querySeq(vkGetPhysicalDeviceSurfaceFormatsKHR(physicalDevice, surface, _, _: VkSurfaceFormatKHR.Buffer))
    val presentModes = querySeq(vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, _, _: IntBuffer))
    SwapChainSupportDetails(capabilities, formats, presentModes)
  end querySwapChainSupport

  def createSwapChain()(using stack: MemoryStack) =
    val swapChainSupport = querySwapChainSupport(physicalDevice)
    import swapChainSupport.capabilities

    // Swapchain Properties
    val surfaceFormat: VkSurfaceFormatKHR = swapChainSupport.formats.find(fmt =>
        fmt.format == VK_FORMAT_B8G8R8A8_SRGB && fmt.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
      ).getOrElse(swapChainSupport.formats.head)
    val presentMode: Int = swapChainSupport.presentModes.find(_ == VK_PRESENT_MODE_MAILBOX_KHR)
      .getOrElse(VK_PRESENT_MODE_FIFO_KHR)
    val extent: VkExtent2D =
      if capabilities.currentExtent.width != Int.MaxValue then capabilities.currentExtent
      else
        val width = stack.mallocInt(1)
        val height = stack.mallocInt(1)
        glfwGetFramebufferSize(window, width, height)
        VkExtent2D.calloc(stack)
          .width(clamp(width.get(0), capabilities.minImageExtent.width, capabilities.maxImageExtent.width))
          .height(clamp(height.get(0), capabilities.minImageExtent.height, capabilities.maxImageExtent.height))

    val imageCount: Int =
      if swapChainSupport.capabilities.maxImageCount > 0
      then
        clamp(swapChainSupport.capabilities.minImageCount + 1,
          swapChainSupport.capabilities.minImageCount,
          swapChainSupport.capabilities.maxImageCount)
      else swapChainSupport.capabilities.minImageCount + 1

    // Swapchain Creation Info
    val createInfo = VkSwapchainCreateInfoKHR.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
      .surface(surface)
      .minImageCount(imageCount)
      .imageFormat(surfaceFormat.format)
      .imageColorSpace(surfaceFormat.colorSpace)
      .imageExtent(extent)
      .imageArrayLayers(1)
      .imageUsage(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)

    val indices = findQueueFamiliesIds(physicalDevice)
    if indices(FamilyType.graphicsFamily) != indices(FamilyType.presentFamily) then
      createInfo.imageSharingMode(VK_SHARING_MODE_CONCURRENT)
      createInfo.pQueueFamilyIndices(asIntBuffer(indices.mapValues(_.head).values.toList, stack))
    else
      createInfo.imageSharingMode(VK_SHARING_MODE_EXCLUSIVE)

    // TODO try putting what goes below at the top of if for prettiness
    createInfo.preTransform(swapChainSupport.capabilities.currentTransform)
    createInfo.compositeAlpha(VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
    createInfo.presentMode(presentMode)
    createInfo.clipped(true)
    createInfo.oldSwapchain(VK_NULL_HANDLE)

    swapChain = create(vkCreateSwapchainKHR(device, createInfo, null, _: LongBuffer))
    swapChainImages = querySeq(vkGetSwapchainImagesKHR(device, swapChain, _, _: LongBuffer))
    swapChainImageFormat = surfaceFormat.format
    swapChainExtent = VkExtent2D.create().set(extent)
  end createSwapChain

  def createImageViews()(using stack: MemoryStack) =
    swapChainImageViews = for img <- swapChainImages yield
      val createInfo = VkImageViewCreateInfo.calloc(stack)
      createInfo.sType(VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
      createInfo.image(img)
      createInfo.viewType(VK_IMAGE_VIEW_TYPE_2D)
      createInfo.format(swapChainImageFormat)
      createInfo.components.r(VK_COMPONENT_SWIZZLE_IDENTITY)
      createInfo.components.g(VK_COMPONENT_SWIZZLE_IDENTITY)
      createInfo.components.b(VK_COMPONENT_SWIZZLE_IDENTITY)
      createInfo.components.a(VK_COMPONENT_SWIZZLE_IDENTITY)
      createInfo.subresourceRange.aspectMask(VK_IMAGE_ASPECT_COLOR_BIT)
      createInfo.subresourceRange.baseMipLevel(0)
      createInfo.subresourceRange.levelCount(1)
      createInfo.subresourceRange.baseArrayLayer(0)
      createInfo.subresourceRange.layerCount(1)

      create(vkCreateImageView(device, createInfo, null, _: LongBuffer))
  end createImageViews

  def createRenderPass()(using stack: MemoryStack) =
    val colorAttachment = VkAttachmentDescription.calloc(1, stack)
      .format(swapChainImageFormat)
      .samples(VK_SAMPLE_COUNT_1_BIT)
      .loadOp(VK_ATTACHMENT_LOAD_OP_CLEAR)
      .storeOp(VK_ATTACHMENT_STORE_OP_STORE)
      .stencilLoadOp(VK_ATTACHMENT_LOAD_OP_DONT_CARE)
      .stencilStoreOp(VK_ATTACHMENT_STORE_OP_DONT_CARE)
      .initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
      .finalLayout(VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)

    val colorAttachmentRef = VkAttachmentReference.calloc(1, stack)
      .attachment(0)
      .layout(VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)

    val subpass = VkSubpassDescription.calloc(1, stack)
      .pipelineBindPoint(VK_PIPELINE_BIND_POINT_GRAPHICS)
      .colorAttachmentCount(1)
      .pColorAttachments(colorAttachmentRef)

    val dependency = VkSubpassDependency.calloc(1, stack)
      .srcSubpass(VK_SUBPASS_EXTERNAL)
        .srcStageMask(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
        .srcAccessMask(0)
      .dstSubpass(0)
        .dstStageMask(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
        .dstAccessMask(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)

    val renderPassInfo = VkRenderPassCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
      .pAttachments(colorAttachment)
      .pSubpasses(subpass)
      .pDependencies(dependency)

    renderPass = create(vkCreateRenderPass(device, renderPassInfo, null, _: LongBuffer))
  end createRenderPass

  def createGraphicsPipeline()(using stack: MemoryStack) =
    // Shader Stages
    def compileShader(shaderFile: String, shaderKind: Int): Long =
      val source = Source.fromResource(s"shaders/$shaderFile").mkString

      val compiler = shaderc_compiler_initialize()
      if compiler == NULL then throw RuntimeException("Failed to create shader compiler")

      val result = shaderc_compile_into_spv(compiler, source, shaderKind, shaderFile, "main", NULL)
      if result == NULL then throw RuntimeException(s"Failed to compile shader $shaderFile into SPIR-V")

      if shaderc_result_get_compilation_status(result) != shaderc_compilation_status_success then
        throw RuntimeException(s"Failed to compile shader $shaderFile into SPIR-V:\n ${shaderc_result_get_error_message(result)}")

      shaderc_compiler_release(compiler)
      result
    end compileShader

    def createShaderModule(shaderPtr: Long): Long =
      val shaderCode = shaderc_result_get_bytes(shaderPtr)
      val createInfo = VkShaderModuleCreateInfo.calloc(stack)
      createInfo.sType(VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
      createInfo.pCode(shaderCode)
      create(vkCreateShaderModule(device, createInfo, null, _: LongBuffer))
    end createShaderModule

    val entryPoint = stack.UTF8("main")
    def populateShaderStage(createInfo: VkPipelineShaderStageCreateInfo, stage: Int, module: Long) =
      createInfo.sType(VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
      createInfo.stage(stage)
      createInfo.module(module)
      createInfo.pName(entryPoint)

    val vertShader = compileShader("25_shader_textures.vert", shaderc_vertex_shader)
    val fragShader = compileShader("25_shader_textures.frag", shaderc_fragment_shader)
    val vertShaderModule = createShaderModule(vertShader)
    val fragShaderModule = createShaderModule(fragShader)

    val shaderStages = VkPipelineShaderStageCreateInfo.callocStack(2, stack)
    populateShaderStage(shaderStages.get(0), VK_SHADER_STAGE_VERTEX_BIT, vertShaderModule)
    populateShaderStage(shaderStages.get(1), VK_SHADER_STAGE_FRAGMENT_BIT, fragShaderModule)

    // Fixed-Function Stages
    val vertexInputInfo = VkPipelineVertexInputStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
      .pVertexBindingDescriptions(Vertex.getBindingDescription())
      .pVertexAttributeDescriptions(Vertex.getAttributeDescriptions())

    val inputAssembly = VkPipelineInputAssemblyStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
      .topology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
      .primitiveRestartEnable(false)

    val viewport = VkViewport.calloc(1, stack)
      .x(0.0f)
      .y(0.0f)
      .width(swapChainExtent.width.toFloat)
      .height(swapChainExtent.height.toFloat)
      .minDepth(0.0f)
      .maxDepth(1.0f)

    val scissor = VkRect2D.calloc(1, stack)
      .offset(VkOffset2D.calloc(stack).set(0, 0))
      .extent(swapChainExtent)

    val viewportState = VkPipelineViewportStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
      .pViewports(viewport)
      .pScissors(scissor)

    val rasterizer = VkPipelineRasterizationStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
      .depthClampEnable(false)
      .rasterizerDiscardEnable(false)
      .polygonMode(VK_POLYGON_MODE_FILL)
      .lineWidth(1.0f)
      .cullMode(VK_CULL_MODE_BACK_BIT)
      .frontFace(VK_FRONT_FACE_COUNTER_CLOCKWISE)
      .depthBiasEnable(false)
      .depthBiasConstantFactor(0.0f)
      .depthBiasClamp(0.0f)
      .depthBiasSlopeFactor(0.0f)

    val multisampling = VkPipelineMultisampleStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
      .sampleShadingEnable(false)
      .rasterizationSamples(VK_SAMPLE_COUNT_1_BIT)
      .minSampleShading(1.0f) // Optional
      .pSampleMask(null) // Optional
      .alphaToCoverageEnable(false) // Optional
      .alphaToOneEnable(false) // Optional

    val colorBlendAttachment = VkPipelineColorBlendAttachmentState.calloc(1, stack)
      .colorWriteMask(VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT)
      .blendEnable(false)

    val colorBlending = VkPipelineColorBlendStateCreateInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
      .logicOpEnable(false)
      .logicOp(VK_LOGIC_OP_COPY) // Optional
      .pAttachments(colorBlendAttachment)
      .blendConstants(stack.floats(0.0f, 0.0f, 0.0f, 0.0f))

    pipelineLayout = create(vkCreatePipelineLayout(device, VkPipelineLayoutCreateInfo.calloc(stack)
        .sType(VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
        .pSetLayouts(stack.longs(descriptorSetLayout)),
      null, _: LongBuffer))

    // Graphics Pipeline
    val pipelineInfo = VkGraphicsPipelineCreateInfo.calloc(1, stack)
      .sType(VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
      .pStages(shaderStages)
      .pVertexInputState(vertexInputInfo)
      .pInputAssemblyState(inputAssembly)
      .pViewportState(viewportState)
      .pRasterizationState(rasterizer)
      .pMultisampleState(multisampling)
      .pDepthStencilState(null) // Optional
      .pColorBlendState(colorBlending)
      .pDynamicState(null) // Optional
      .layout(pipelineLayout)
      .renderPass(renderPass)
      .subpass(0)
      .basePipelineHandle(VK_NULL_HANDLE) // Optional
      .basePipelineIndex(-1) // Optional

    graphicsPipeline = create(vkCreateGraphicsPipelines(
      device, VK_NULL_HANDLE, pipelineInfo, null, _: LongBuffer))

    // Clean-up
    vkDestroyShaderModule(device, fragShaderModule, null)
    vkDestroyShaderModule(device, vertShaderModule, null)
    shaderc_result_release(vertShader)
    shaderc_result_release(fragShader)
  end createGraphicsPipeline

  def createFramebuffers()(using stack: MemoryStack) =
    val attachments = stack.mallocLong(1)

    val framebufferInfo = VkFramebufferCreateInfo.callocStack(stack)
    framebufferInfo.sType(VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
    framebufferInfo.renderPass(renderPass)
    framebufferInfo.width(swapChainExtent.width)
    framebufferInfo.height(swapChainExtent.height)
    framebufferInfo.layers(1)

    swapChainFramebuffers = for imageView <- swapChainImageViews yield
      attachments.put(0, imageView)
      framebufferInfo.pAttachments(attachments)  // TODO: do we really need to re-bound this again?..
      create(vkCreateFramebuffer(device, framebufferInfo, null, _: LongBuffer))
  end createFramebuffers

  def cleanupSwapChain() =
    swapChainFramebuffers.foreach(vkDestroyFramebuffer(device, _, null))
    vkDestroyPipeline(device, graphicsPipeline, null)
    vkDestroyPipelineLayout(device, pipelineLayout, null)
    vkDestroyRenderPass(device, renderPass, null)
    swapChainImageViews.foreach(vkDestroyImageView(device, _, null))
    vkDestroySwapchainKHR(device, swapChain, null)
  end cleanupSwapChain

  def initializeSwapChain()(using stack: MemoryStack) =
    createSwapChain()
    createImageViews()
    createRenderPass()
    createGraphicsPipeline()
    createFramebuffers()
  end initializeSwapChain

  def recreateSwapChain()(using stack: MemoryStack) =
    cleanupSwapChain()
    initializeSwapChain()
  end recreateSwapChain
end SwapChainLifecycle

def loop() =
  var currentFrame = 0
  var recreate = true
  def drawFrame(): Unit = Using.resource(stackPush()) { stack =>
    given MemoryStack = stack
    def recordCommandBuffer(commandBuffer: VkCommandBuffer, imageIndex: Int) =
      val beginInfo = VkCommandBufferBeginInfo.callocStack(stack)
        .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)

      val renderPassInfo = VkRenderPassBeginInfo.callocStack(stack)
        .sType(VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
        .renderPass(renderPass)
        .renderArea(VkRect2D.callocStack(stack)
          .offset(VkOffset2D.callocStack(stack).set(0, 0))
          .extent(swapChainExtent))
        .pClearValues(VkClearValue.callocStack(1, stack)
          .color(VkClearColorValue.calloc(stack)
            .float32(stack.floats(0.0f, 0.0f, 0.0f, 1.0f))))
        .framebuffer(swapChainFramebuffers(imageIndex))

      checkSuccess(vkBeginCommandBuffer(commandBuffer, beginInfo))
      vkCmdBeginRenderPass(commandBuffer, renderPassInfo, VK_SUBPASS_CONTENTS_INLINE)

      vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline)

      vkCmdBindVertexBuffers(commandBuffer, 0, stack.longs(vertexBuffer), stack.longs(0))
      vkCmdBindIndexBuffer(commandBuffer, indexBuffer, 0, VK_INDEX_TYPE_UINT16)
      vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
        pipelineLayout, 0, stack.longs(descriptorSets(currentFrame)), null)

      vkCmdDrawIndexed(commandBuffer, indices.length, 1, 0, 0, 0)

      vkCmdEndRenderPass(commandBuffer)
      checkSuccess(vkEndCommandBuffer(commandBuffer))
    end recordCommandBuffer

    def updateUniformBuffer() =
      val ubo = UniformBufferObject(Matrix4f(), Matrix4f(), Matrix4f())
      ubo.model.rotate((glfwGetTime() * Math.toRadians(90)).toFloat, 0.0f, 0.0f, 1.0f);
      ubo.view.lookAt(
        Vector3f(2.0f, 2.0f, 2.0f),
        Vector3f(0.0f, 0.0f, 0.0f),
        Vector3f(0.0f, 0.0f, 1.0f))
      ubo.proj.perspective(Math.toRadians(45).toFloat,
        (swapChainExtent.width / swapChainExtent.height).toFloat, 0.1f, 10.0f);
      ubo.proj.m11(ubo.proj.m11() * -1)

      val uboMemory = uniformBuffersMemory(currentFrame)
      val data = queryStruct(vkMapMemory(device, uboMemory,
          0, UniformBufferObject.size, 0, _: PointerBuffer))
        .getByteBuffer(0, UniformBufferObject.size)
      val mat4Size = 16 * java.lang.Float.BYTES
      ubo.model.get(0, data)
      ubo.view.get(alignas(mat4Size, 4 * java.lang.Float.BYTES), data)
      ubo.proj.get(alignas(mat4Size * 2, 4 * java.lang.Float.BYTES), data)
      vkUnmapMemory(device, uboMemory)
    end updateUniformBuffer

    val inFlightFence = inFlightFences(currentFrame)
    val imageAvailableSemaphore = imageAvailableSemaphores(currentFrame)
    val renderFinishedSemaphore = renderFinishedSemaphores(currentFrame)
    val commandBuffer = commandBuffers(currentFrame)

    vkWaitForFences(device, inFlightFence, true, Long.MaxValue)

    val imageIndex =
      try query(vkAcquireNextImageKHR(device, swapChain, Long.MaxValue, imageAvailableSemaphore, VK_NULL_HANDLE, _: IntBuffer))
      catch
        case QueryException(VK_ERROR_OUT_OF_DATE_KHR) | QueryException(VK_SUBOPTIMAL_KHR) =>
          println("Recreating swap chain")
          SwapChainLifecycle.recreateSwapChain()
          return

    vkResetFences(device, inFlightFence)

    vkResetCommandBuffer(commandBuffer, 0)
    recordCommandBuffer(commandBuffer, imageIndex)

    updateUniformBuffer()

    val submitInfo = VkSubmitInfo.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_SUBMIT_INFO)
      .pWaitSemaphores(stack.longs(imageAvailableSemaphore))
      .pWaitDstStageMask(stack.ints(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT))
      .pCommandBuffers(stack.pointers(commandBuffer))
      .pSignalSemaphores(stack.longs(renderFinishedSemaphore))
    checkSuccess(vkQueueSubmit(graphicsQueue, submitInfo, inFlightFence))

    val presentInfo = VkPresentInfoKHR.calloc(stack)
      .sType(VK_STRUCTURE_TYPE_PRESENT_INFO_KHR)
      .pWaitSemaphores(stack.longs(renderFinishedSemaphore))
      .swapchainCount(1)
      .pSwapchains(stack.longs(swapChain))
      .pImageIndices(stack.ints(imageIndex))
    vkQueuePresentKHR(presentQueue, presentInfo)

    currentFrame = (currentFrame + 1) % maxFramesInFlight
  }

  while !glfwWindowShouldClose(window) do
    glfwPollEvents()
    drawFrame()
end loop

def cleanup() =
  SwapChainLifecycle.cleanupSwapChain()

  vkDestroySampler(device, textureSampler, null)
  vkDestroyImageView(device, textureImageView, null)
  vkDestroyImage(device, textureImage, null)
  vkFreeMemory(device, textureImageMemory, null)
  vkDestroyDescriptorPool(device, descriptorPool, null)
  vkDestroyDescriptorSetLayout(device, descriptorSetLayout, null)
  uniformBuffers.foreach(vkDestroyBuffer(device, _, null))
  uniformBuffersMemory.foreach(vkFreeMemory(device, _, null))
  vkDestroyBuffer(device, indexBuffer, null)
  vkFreeMemory(device, indexBufferMemory, null)
  vkDestroyBuffer(device, vertexBuffer, null)
  vkFreeMemory(device, vertexBufferMemory, null)

  imageAvailableSemaphores.foreach(vkDestroySemaphore(device, _, null))
  renderFinishedSemaphores.foreach(vkDestroySemaphore(device, _, null))
  inFlightFences.foreach(vkDestroyFence(device, _, null))

  vkDestroyCommandPool(device, commandPool, null)
  vkDestroyDevice(device, null)
  if enableValidationLayers then
    vkDestroyDebugUtilsMessengerEXT(instance, debugMessenger, null)
  vkDestroySurfaceKHR(instance, surface, null)
  vkDestroyInstance(instance, null)
  glfwDestroyWindow(window)
  glfwTerminate()


// === Utility ===

def asPointerBuffer(strs: Seq[String], stack: MemoryStack): PointerBuffer =
  val buffer = stack.mallocPointer(strs.length)
  for str <- strs do buffer.put(stack.UTF8(str))
  buffer.rewind()

def asIntBuffer(ints: Seq[Int], stack: MemoryStack): IntBuffer =
  val buffer = stack.mallocInt(ints.length)
  for i <- ints do buffer.put(i)
  buffer.rewind().asInstanceOf[IntBuffer]

extension [T] (buf: java.nio.Buffer { def get(): T })
  def toList: List[T] =
    val bldr = ListBuffer.empty[T]
    while buf.hasRemaining do bldr += buf.get()
    bldr.toList

def clamp(x: Int, min: Int, max: Int) =
  assert(min <= max)
  if x >= min && x <= max then x
  else if x > max then max
  else min

def alignas(offset: Int, alignment: Int): Int =
  if offset % alignment == 0 then offset else ((offset - 1) | (alignment - 1)) + 1


// === Memory operations ===

trait Allocatable[T]:
  def apply(size: Int)(using MemoryStack): T

def alloc[T](size: Int = 1)(using stk: MemoryStack, tcl: Allocatable[T]) = tcl(size)

object Allocatable:  // TODO can be simplified via macros
  def make[T](f: (Int, MemoryStack) => T) = new Allocatable[T] {
    def apply(size: Int)(using stk: MemoryStack) = f(size, stk) }
  given Allocatable[PointerBuffer] = make((size, stk) => stk.mallocPointer(size))
  given Allocatable[LongBuffer] = make((size, stk) => stk.mallocLong(size))
  given Allocatable[IntBuffer] = make((size, stk) => stk.mallocInt(size))
  given surfBuf: Allocatable[VkSurfaceFormatKHR.Buffer] = make(VkSurfaceFormatKHR.calloc)
  given famiBuf: Allocatable[VkQueueFamilyProperties.Buffer] = make(VkQueueFamilyProperties.calloc)
  given layrBuf: Allocatable[VkLayerProperties.Buffer] = make(VkLayerProperties.calloc)
  given extPBuf: Allocatable[VkExtensionProperties.Buffer] = make(VkExtensionProperties.calloc)
  given scpbBuf: Allocatable[VkSurfaceCapabilitiesKHR] = make((_, stack) => VkSurfaceCapabilitiesKHR.calloc(stack))
  given memrBuf: Allocatable[VkMemoryRequirements] = make((_, stack) => VkMemoryRequirements.calloc(stack))
  given pdmpBuf: Allocatable[VkPhysicalDeviceMemoryProperties] = make((_, stack) => VkPhysicalDeviceMemoryProperties.calloc(stack))
  given pdftBuf: Allocatable[VkPhysicalDeviceFeatures] = make((_, stack) => VkPhysicalDeviceFeatures.callocStack(stack))

trait AsScalaList[T, C]:
  def toList(c: C): List[T]

extension [C](c: C) def toList[T](using conv: AsScalaList[T, C]): List[T] =
  conv.toList(c)

object AsScalaList:
  given bufferAsList[T, C <: { def get(): T; def hasRemaining(): Boolean }]: AsScalaList[T, C] = buf => {
    val bldr = ListBuffer.empty[T]
    while buf.hasRemaining() do bldr += buf.get()
    bldr.toList
  }

def checkSuccess(f: => Int, message: String = "Vulkan method failed"): Int =
  val res = f
  if res != VK_SUCCESS then throw RuntimeException(message)
  res

type Buf[T] = { def get(i: Int): T }
def createMany[T, Ptr <: Buf[T]: Allocatable: AsScalaList[T, _]](function: Ptr => Int, count: Int)(using MemoryStack): List[T] =
  val ptr: Ptr = alloc(count)
  checkSuccess(function(ptr), "Failed to create a Vulkan object")
  ptr.toList

def create[T, Ptr <: Buf[T]: Allocatable: AsScalaList[T, _]](function: Ptr => Int)(using MemoryStack): T =
  createMany(function, 1).head

def createManyLooped[T, Ptr <: Buf[T]: Allocatable: AsScalaList[T, _]](function: Ptr => Int, count: Int)(using MemoryStack): List[T] =
  (for _ <- 0 until count yield create(function)).toList

def querySeq[T, TgtBuf: AsScalaList[T, _]: Allocatable](function: (IntBuffer, TgtBuf | Null) => Int | Unit)(using MemoryStack) =
  val count: IntBuffer = alloc()
  function(count, null)
  val targetBuf: TgtBuf = alloc(count.get(0))
  function(count, targetBuf)
  targetBuf.toList

case class QueryException(status: Int) extends RuntimeException(s"Query failed with status $status")
def query[T, Ptr <: Buf[T]: Allocatable](function: Ptr => Int | Unit)(using MemoryStack) =
  val ptr: Ptr = alloc()
  function(ptr) match
    case status: Int if status != VK_SUCCESS => throw QueryException(status)
    case _ => ptr.get(0)

def queryStruct[T: Allocatable](function: T => Int | Unit)(using MemoryStack) =
  val struct: T = alloc()
  function(struct)
  struct
