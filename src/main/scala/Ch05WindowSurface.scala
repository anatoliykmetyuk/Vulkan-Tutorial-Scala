package ch05WindowSurface

import scala.util.Using
import scala.jdk.CollectionConverters._

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWVulkan.*

import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.EXTDebugUtils.*
import org.lwjgl.vulkan.KHRSurface.*

import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.PointerBuffer


var window = -1l
var instance: VkInstance = null
var physicalDevice: VkPhysicalDevice = null
var device: VkDevice = null
var surface: Long = -1l
var graphicsQueue: VkQueue = null
var presentQueue: VkQueue = null
var debugMessenger: Long = -1l
val validationLayers = List("VK_LAYER_KHRONOS_validation")
val enableValidationLayers = true


@main def Ch05WindowSurface =
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
      extensions
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
      val validationLayersBuf = stack.callocPointer(validationLayers.length)
      for l <- validationLayers do validationLayersBuf.put(stack.UTF8(l))
      createInfo.ppEnabledLayerNames(validationLayersBuf)
      createInfo.pNext(debugMessengerCreateInfo)


    val instancePointer = stack.mallocPointer(1)
    if vkCreateInstance(createInfo, null, instancePointer) != VK_SUCCESS then
      throw RuntimeException("Failed to create the Vulkan instance")
    instance = VkInstance(instancePointer.get(0), createInfo)
  end createInstance

  def setupDebugMessenger() =
    val debugMessengerBuf = stack.mallocLong(1)
    if vkCreateDebugUtilsMessengerEXT(instance, debugMessengerCreateInfo, null, debugMessengerBuf) != VK_SUCCESS then
      throw RuntimeException("failed to set up debug messenger!")
    debugMessenger = debugMessengerBuf.get(0)
  end setupDebugMessenger

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

  def createSurface() =
    val surfacePtr = stack.mallocLong(1)
    if glfwCreateWindowSurface(instance, window, null, surfacePtr) != VK_SUCCESS then
      throw RuntimeException("failed to create window surface!")
    surface = surfacePtr.get(0)
  end createSurface

  def pickPhysicalDevice() =
    def isDeviceSuitable(device: VkPhysicalDevice): Boolean =
      val fams = findQueueFamiliesIds(device)
      fams.contains(FamilyType.graphicsFamily) &&
        fams.contains(FamilyType.presentFamily)
    end isDeviceSuitable

    val deviceCount = stack.mallocInt(1)
    vkEnumeratePhysicalDevices(instance, deviceCount, null)

    val devicePtrs = stack.mallocPointer(deviceCount.get(0))
    vkEnumeratePhysicalDevices(instance, deviceCount, devicePtrs)
    val devices = for i <- 0 until deviceCount.get(0)
      yield VkPhysicalDevice(devicePtrs.get(i), instance)

    physicalDevice = devices.find(isDeviceSuitable).get

    println(s"All devices: ${devices.mkString(", ")}\nSelected device: $physicalDevice")
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

    val pDevice = stack.mallocPointer(1)
    if vkCreateDevice(physicalDevice, createInfo, null, pDevice) != VK_SUCCESS then
      throw RuntimeException("Failed to create logical device")
    device = VkDevice(pDevice.get(0), physicalDevice, createInfo)

    val pQueue = stack.mallocPointer(1)
    vkGetDeviceQueue(device, graphicsFamilyId, 0, pQueue)
    graphicsQueue = VkQueue(pQueue.get(0), device)

    vkGetDeviceQueue(device, presentFamilyId, 0, pQueue)
    presentQueue = VkQueue(pQueue.get(0), device)
  end createLogicalDevice

  createInstance()
  if enableValidationLayers then setupDebugMessenger()
  createSurface()
  pickPhysicalDevice()
  createLogicalDevice()
}

def loop() =
  while !glfwWindowShouldClose(window) do
    glfwPollEvents()

def cleanup() =
  vkDestroyDevice(device, null)
  if enableValidationLayers then
    vkDestroyDebugUtilsMessengerEXT(instance, debugMessenger, null)
  vkDestroySurfaceKHR(instance, surface, null)
  vkDestroyInstance(instance, null)
  glfwDestroyWindow(window)
  glfwTerminate()
