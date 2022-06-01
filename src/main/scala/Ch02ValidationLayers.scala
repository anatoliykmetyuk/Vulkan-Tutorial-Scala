package ch02ValidationLayers

import scala.util.Using
import scala.jdk.CollectionConverters._

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWVulkan.*

import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.EXTDebugUtils.*

import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.PointerBuffer


var window = -1l
var instance: VkInstance = null
var debugMessenger: Long = -1l
val validationLayers = List("VK_LAYER_KHRONOS_validation")
val enableValidationLayers = true


@main def Ch02ValidationLayers =
  initWindow()
  initVulkan()
  loop()
  cleanup()

def initWindow() =
  if !glfwInit() then throw RuntimeException("Cannot init GLFW")
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
  window = glfwCreateWindow(800, 600, "Vulkan", 0, 0)
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

  createInstance()
  if enableValidationLayers then setupDebugMessenger()
}

def loop() =
  while !glfwWindowShouldClose(window) do
    glfwPollEvents()

def cleanup() =
  if enableValidationLayers then
    vkDestroyDebugUtilsMessengerEXT(instance, debugMessenger, null)
  vkDestroyInstance(instance, null)
  glfwDestroyWindow(window)
  glfwTerminate()
