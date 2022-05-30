import scala.util.Using

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWVulkan.*

import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.VK10.*

import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.PointerBuffer


var window = -1l
var instance: VkInstance = null

@main def Ch1InstanceCreation =
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

def initVulkan() =
  createInstance()

def createInstance() =
  Using(stackPush()) { stack =>
    val appInfo = VkApplicationInfo.calloc(stack)
    appInfo.sType(VK_STRUCTURE_TYPE_APPLICATION_INFO)
    appInfo.pApplicationName(memASCII("Hello Triangle"))
    appInfo.applicationVersion(VK_MAKE_VERSION(1, 0, 0))
    appInfo.pEngineName(memASCII("No Engine"))
    appInfo.engineVersion(VK_MAKE_VERSION(1, 0, 0))
    appInfo.apiVersion(VK_API_VERSION_1_0)

    val createInfo = VkInstanceCreateInfo.calloc(stack)
    createInfo.sType(VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
    createInfo.pApplicationInfo(appInfo)
    createInfo.ppEnabledExtensionNames(glfwGetRequiredInstanceExtensions())

    val instancePointer = stack.mallocPointer(1)
    if vkCreateInstance(createInfo, null, instancePointer) != VK_SUCCESS then
      throw RuntimeException("Failed to create the Vulkan instance")
    instance = VkInstance(instancePointer.get(0), createInfo)
  }


def loop() =
  while !glfwWindowShouldClose(window) do
    glfwPollEvents()

def cleanup() =
  vkDestroyInstance(instance, null)
  glfwDestroyWindow(window)
  glfwTerminate()
