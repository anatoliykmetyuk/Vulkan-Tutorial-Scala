package ch06SwapChainCreation

import reflect.Selectable.reflectiveSelectable

import scala.util.Using
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
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.Buffer
import scala.collection.JavaConverters.AsScala


var window = -1l
var instance: VkInstance = null
var physicalDevice: VkPhysicalDevice = null
var device: VkDevice = null
var surface: Long = -1l
var graphicsQueue: VkQueue = null
var presentQueue: VkQueue = null
var debugMessenger: Long = -1l
var swapChain: Long = -1l
var swapChainImages: List[Long] = List.empty
var swapChainImageFormat: Int = -1
var swapChainExtent: VkExtent2D = null

val enableValidationLayers = true

val validationLayers = List("VK_LAYER_KHRONOS_validation")
val deviceExtensions = List(VK_KHR_SWAPCHAIN_EXTENSION_NAME)

@main def Ch06SwapChainCreation =
  initWindow()
  initVulkan()
  loop()
  cleanup()

def initWindow() =
  if !glfwInit() then throw RuntimeException("Cannot init GLFW")
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
  window = glfwCreateWindow(800, 600, this.getClass.getSimpleName, 0, 0)
  if window == 0 then throw RuntimeException("Cannot create window")

def initVulkan() = Using.resource(stackPush()) { stack =>
  given MemoryStack = stack

  def checkValidationLayerSupport() =
    val layerCount = stack.mallocInt(1)
    vkEnumerateInstanceLayerProperties(layerCount, null)

    val availableLayersBuf = VkLayerProperties.calloc(layerCount.get(0), stack)
    vkEnumerateInstanceLayerProperties(layerCount, availableLayersBuf)
    val availableLayers = availableLayersBuf.asScala.map(_.layerNameString).toSet

    val unavailableLayers = validationLayers.filterNot(availableLayers)
    if unavailableLayers.nonEmpty then
      throw RuntimeException(s"The following layers are not supported: ${unavailableLayers.mkString(", ")}")
  end checkValidationLayerSupport

  def getRequiredExtensions() =
    val glfwExtensions: PointerBuffer = glfwGetRequiredInstanceExtensions()
    if enableValidationLayers then
      val extensions: PointerBuffer = stack.mallocPointer(glfwExtensions.capacity() + 1)

      for i <- 0 until glfwExtensions.capacity do
        extensions.put(i, glfwExtensions.get(i))
      extensions.put(glfwExtensions.capacity, stack.UTF8(VK_EXT_DEBUG_UTILS_EXTENSION_NAME))
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
    createInfo.sType(VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
    createInfo.pApplicationInfo(appInfo)
    createInfo.ppEnabledExtensionNames(getRequiredExtensions())
    if enableValidationLayers then
      checkValidationLayerSupport()
      createInfo.ppEnabledLayerNames(asPointerBuffer(validationLayers, stack))
      createInfo.pNext(debugMessengerCreateInfo)

    instance = VkInstance(create(vkCreateInstance(createInfo, null, _)), createInfo)
  end createInstance

  enum FamilyType { case graphicsFamily, presentFamily }
  def findQueueFamiliesIds(device: VkPhysicalDevice): Map[FamilyType, List[Int]] =
    val queueFamilyCount = stack.mallocInt(1)
    vkGetPhysicalDeviceQueueFamilyProperties(device, queueFamilyCount, null)

    val queueFamiliesProps = VkQueueFamilyProperties.calloc(queueFamilyCount.get(0), stack)
    vkGetPhysicalDeviceQueueFamilyProperties(device, queueFamilyCount, queueFamiliesProps)

    def familiesSupported(props: VkQueueFamilyProperties, qid: Int): List[FamilyType] =
      def presentation(): Option[FamilyType] =
        val presentSupportPtr = stack.mallocInt(1)
        vkGetPhysicalDeviceSurfaceSupportKHR(device, qid, surface, presentSupportPtr)
        if presentSupportPtr.get(0) == 1 then Some(FamilyType.presentFamily) else None

      def graphics(): Option[FamilyType] =
        if (props.queueFlags & VK_QUEUE_GRAPHICS_BIT) != 0 then Some(FamilyType.graphicsFamily) else None

      Nil ++ presentation() ++ graphics()
    end familiesSupported

    ( for
        (props, qid) <- queueFamiliesProps.asScala.toList.zipWithIndex
        familyTpe <- familiesSupported(props, qid)
      yield
        familyTpe -> qid
    ).groupMap(_._1)(_._2)  // familyTpe -> List[queueId]
  end findQueueFamiliesIds

  case class SwapChainSupportDetails(
    capabilities: VkSurfaceCapabilitiesKHR,
    formats: List[VkSurfaceFormatKHR],
    presentModes: List[Int],
  )

  def querySwapChainSupport(physicalDevice: VkPhysicalDevice): SwapChainSupportDetails =
    val capabilities = VkSurfaceCapabilitiesKHR.calloc(stack)
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface, capabilities)

    val formats = querySeq(vkGetPhysicalDeviceSurfaceFormatsKHR(physicalDevice, surface, _, _: VkSurfaceFormatKHR.Buffer))

    val presentModeCount = stack.mallocInt(1)
    vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, presentModeCount, null)
    val presentModes = stack.mallocInt(presentModeCount.get(0))
    vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, presentModeCount, presentModes)

    SwapChainSupportDetails(capabilities, formats, presentModes.toList)
  end querySwapChainSupport

  def pickPhysicalDevice() =
    def isDeviceSuitable(device: VkPhysicalDevice): Boolean =
      def checkQueueSupport() =
        val fams = findQueueFamiliesIds(device)
        fams.contains(FamilyType.graphicsFamily) &&
          fams.contains(FamilyType.presentFamily)
      end checkQueueSupport

      def checkDeviceExtensionSupport(): Boolean =
        val extensionsCountPtr = stack.mallocInt(1)
        vkEnumerateDeviceExtensionProperties(device, null: CharSequence, extensionsCountPtr, null)

        val availableExtensionsBuf = VkExtensionProperties.calloc(extensionsCountPtr.get(0), stack)
        vkEnumerateDeviceExtensionProperties(device, null: CharSequence, extensionsCountPtr, availableExtensionsBuf)
        val availableExtensions = availableExtensionsBuf.asScala.toSet.map(_.extensionNameString)

        deviceExtensions.forall(availableExtensions.contains)
      end checkDeviceExtensionSupport

      def checkSwapChainSupport(): Boolean =
        val swapChainSupport = querySwapChainSupport(device)
        swapChainSupport.formats.nonEmpty && swapChainSupport.presentModes.nonEmpty
      end checkSwapChainSupport

      checkQueueSupport() && checkDeviceExtensionSupport() && checkSwapChainSupport()
    end isDeviceSuitable

    val deviceCount = stack.mallocInt(1)
    vkEnumeratePhysicalDevices(instance, deviceCount, null)

    val devicePtrs = stack.mallocPointer(deviceCount.get(0))
    vkEnumeratePhysicalDevices(instance, deviceCount, devicePtrs)
    val devices = for i <- 0 until deviceCount.get(0)
      yield VkPhysicalDevice(devicePtrs.get(i), instance)

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
    createInfo.pEnabledFeatures(VkPhysicalDeviceFeatures.calloc(stack))
    createInfo.ppEnabledExtensionNames(asPointerBuffer(deviceExtensions, stack))
    if enableValidationLayers then
      createInfo.ppEnabledLayerNames(asPointerBuffer(validationLayers, stack))
    device = VkDevice(create(vkCreateDevice(physicalDevice, createInfo, null, _)), physicalDevice, createInfo)

    val pQueue = stack.mallocPointer(1)
    vkGetDeviceQueue(device, graphicsFamilyId, 0, pQueue)
    graphicsQueue = VkQueue(pQueue.get(0), device)

    vkGetDeviceQueue(device, presentFamilyId, 0, pQueue)
    presentQueue = VkQueue(pQueue.get(0), device)
  end createLogicalDevice

  def createSwapChain() =
    val swapChainSupport = querySwapChainSupport(physicalDevice)

    // Swapchain Properties
    val surfaceFormat: VkSurfaceFormatKHR = swapChainSupport.formats.find(fmt =>
        fmt.format == VK_FORMAT_B8G8R8A8_SRGB && fmt.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
      ).getOrElse(swapChainSupport.formats.head)
    val presentMode: Int = swapChainSupport.presentModes.find(_ == VK_PRESENT_MODE_MAILBOX_KHR)
      .getOrElse(VK_PRESENT_MODE_FIFO_KHR)
    val extent: VkExtent2D =
      import swapChainSupport.capabilities
      if capabilities.currentExtent.width != Int.MaxValue then capabilities.currentExtent
      else
        val width = stack.mallocInt(1)
        val height = stack.mallocInt(1)
        glfwGetFramebufferSize(window, width, height)

        val actualExtent = VkExtent2D.calloc(stack)
        actualExtent.width(clamp(width.get(0), capabilities.minImageExtent.width, capabilities.maxImageExtent.width))
        actualExtent.height(clamp(height.get(0), capabilities.minImageExtent.height, capabilities.maxImageExtent.height))
        actualExtent
    end extent
    val imageCount: Int =
      if swapChainSupport.capabilities.maxImageCount > 0
      then
        clamp(swapChainSupport.capabilities.minImageCount + 1,
          swapChainSupport.capabilities.minImageCount,
          swapChainSupport.capabilities.maxImageCount)
      else swapChainSupport.capabilities.minImageCount + 1

    // Swapchain Creation Info
    val createInfo = VkSwapchainCreateInfoKHR.calloc(stack)
    createInfo.sType(VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
    createInfo.surface(surface)
    createInfo.minImageCount(imageCount)
    createInfo.imageFormat(surfaceFormat.format)
    createInfo.imageColorSpace(surfaceFormat.colorSpace)
    createInfo.imageExtent(extent)
    createInfo.imageArrayLayers(1)
    createInfo.imageUsage(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)

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

    val imageCountReal = stack.mallocInt(1)
    vkGetSwapchainImagesKHR(device, swapChain, imageCountReal, null);
    val images = stack.mallocLong(imageCountReal.get(0))
    vkGetSwapchainImagesKHR(device, swapChain, imageCountReal, images)
    swapChainImages = images.toList

    swapChainImageFormat = surfaceFormat.format
    swapChainExtent = extent
  end createSwapChain

  createInstance()
  if enableValidationLayers then
    debugMessenger = create(vkCreateDebugUtilsMessengerEXT(instance, debugMessengerCreateInfo, null, _: LongBuffer))
  surface = create(glfwCreateWindowSurface(instance, window, null, _: LongBuffer))
  pickPhysicalDevice()
  createLogicalDevice()
  createSwapChain()
}

def loop() =
  while !glfwWindowShouldClose(window) do
    glfwPollEvents()

def cleanup() =
  vkDestroySwapchainKHR(device, swapChain, null)
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


// === Memory operations ===

trait Allocatable[T]:
  def apply(size: Int = 1)(using MemoryStack): T

object Allocatable:
  def make[T](f: (Int, MemoryStack) => T) = new Allocatable[T] {
    def apply(size: Int)(using stk: MemoryStack) = f(size, stk) }
  given Allocatable[PointerBuffer] = make((size, stk) => stk.mallocPointer(size))
  given Allocatable[LongBuffer] = make((size, stk) => stk.mallocLong(size))
  given Allocatable[VkSurfaceFormatKHR.Buffer] = make(VkSurfaceFormatKHR.calloc)

trait AsScalaList[T, C]:
  def toList(c: C): List[T]

extension [C](c: C) def toList[T](using conv: AsScalaList[T, C]): List[T] =
  conv.toList(c)

object AsScalaList:
  given bufferAsList[T, C <: Buffer { def get(): T }]: AsScalaList[T, C] = buf => {
    val bldr = ListBuffer.empty[T]
    while buf.hasRemaining do bldr += buf.get()
    bldr.toList
  }
  given iterableAsList[T, C <: java.lang.Iterable[T]]: AsScalaList[T, C] = _.asScala.toList

type Buf[T] = { def get(i: Int): T }
def create[T, Ptr <: Buf[T]](function: Ptr => Int)(using stk: MemoryStack, alloc: Allocatable[Ptr]): T =
  val ptr = alloc()
  if function(ptr) != VK_SUCCESS then
    throw RuntimeException(s"Failed to create a Vulkan object")
  ptr.get(0)

def querySeq[T, TgtBuf: AsScalaList[T, ?]](function: (IntBuffer, TgtBuf | Null) => Int)(using stk: MemoryStack, alloc: Allocatable[TgtBuf]) =
  val count = stk.mallocInt(1)
  function(count, null)
  val targetBuf = alloc(count.get(0))
  function(count, targetBuf)
  targetBuf.toList

    // val presentModeCount = stack.mallocInt(1)
    // vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, presentModeCount, null)
    // val presentModes = stack.mallocInt(presentModeCount.get(0))
    // vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, presentModeCount, presentModes)


