package ch11RenderPasses

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

import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.Buffer
import java.nio.ByteBuffer


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
var swapChainImageViews: List[Long] = List.empty

var renderPass: Long = -1
var pipelineLayout: Long = -1

val enableValidationLayers = true

val validationLayers = List("VK_LAYER_KHRONOS_validation")
val deviceExtensions = List(VK_KHR_SWAPCHAIN_EXTENSION_NAME)

@main def Ch11RenderPasses =
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
    val availableLayers = querySeq(vkEnumerateInstanceLayerProperties).map(_.layerNameString).toSet
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

  case class SwapChainSupportDetails(
    capabilities: VkSurfaceCapabilitiesKHR,
    formats: List[VkSurfaceFormatKHR],
    presentModes: List[Int],
  )

  def querySwapChainSupport(physicalDevice: VkPhysicalDevice): SwapChainSupportDetails =
    val capabilities = queryStruct(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice, surface, _))
    val formats = querySeq(vkGetPhysicalDeviceSurfaceFormatsKHR(physicalDevice, surface, _, _: VkSurfaceFormatKHR.Buffer))
    val presentModes = querySeq(vkGetPhysicalDeviceSurfacePresentModesKHR(physicalDevice, surface, _, _: IntBuffer))
    SwapChainSupportDetails(capabilities, formats, presentModes)
  end querySwapChainSupport

  def pickPhysicalDevice() =
    def isDeviceSuitable(device: VkPhysicalDevice): Boolean =
      def checkQueueSupport() =
        val fams = findQueueFamiliesIds(device)
        fams.contains(FamilyType.graphicsFamily) &&
          fams.contains(FamilyType.presentFamily)
      end checkQueueSupport

      def checkDeviceExtensionSupport(): Boolean =
        val availableExtensions = querySeq(vkEnumerateDeviceExtensionProperties(device, null: CharSequence, _, _: VkExtensionProperties.Buffer))
          .toSet.map(_.extensionNameString)
        deviceExtensions.forall(availableExtensions.contains)
      end checkDeviceExtensionSupport

      def checkSwapChainSupport(): Boolean =
        val swapChainSupport = querySwapChainSupport(device)
        swapChainSupport.formats.nonEmpty && swapChainSupport.presentModes.nonEmpty
      end checkSwapChainSupport

      checkQueueSupport() && checkDeviceExtensionSupport() && checkSwapChainSupport()
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
    createInfo.pEnabledFeatures(VkPhysicalDeviceFeatures.calloc(stack))
    createInfo.ppEnabledExtensionNames(asPointerBuffer(deviceExtensions, stack))
    if enableValidationLayers then
      createInfo.ppEnabledLayerNames(asPointerBuffer(validationLayers, stack))

    device = VkDevice(create(vkCreateDevice(physicalDevice, createInfo, null, _)), physicalDevice, createInfo)
    graphicsQueue = VkQueue(query(vkGetDeviceQueue(device, graphicsFamilyId, 0, _: PointerBuffer)), device)
    presentQueue = VkQueue(query(vkGetDeviceQueue(device, presentFamilyId, 0, _: PointerBuffer)), device)
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
    swapChainImages = querySeq(vkGetSwapchainImagesKHR(device, swapChain, _, _: LongBuffer))
    swapChainImageFormat = surfaceFormat.format
    swapChainExtent = extent
  end createSwapChain

  def createImageViews() =
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

  def createRenderPass() =
    val colorAttachment: VkAttachmentDescription.Buffer =
      val res = VkAttachmentDescription.calloc(1, stack)
      res.format(swapChainImageFormat)
      res.samples(VK_SAMPLE_COUNT_1_BIT)
      res.loadOp(VK_ATTACHMENT_LOAD_OP_CLEAR)
      res.storeOp(VK_ATTACHMENT_STORE_OP_STORE)
      res.stencilLoadOp(VK_ATTACHMENT_LOAD_OP_DONT_CARE)
      res.stencilStoreOp(VK_ATTACHMENT_STORE_OP_DONT_CARE)
      res.initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
      res.finalLayout(VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
      res

    val colorAttachmentRef: VkAttachmentReference.Buffer =
      val res = VkAttachmentReference.calloc(1, stack)
      res.attachment(0)
      res.layout(VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
      res

    val subpass: VkSubpassDescription.Buffer =
      val res = VkSubpassDescription.calloc(1, stack)
      res.pipelineBindPoint(VK_PIPELINE_BIND_POINT_GRAPHICS)
      res.colorAttachmentCount(1)
      res.pColorAttachments(colorAttachmentRef)
      res

    val renderPassInfo: VkRenderPassCreateInfo =
      val res = VkRenderPassCreateInfo.calloc(stack)
      res.sType(VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
      res.pAttachments(colorAttachment)
      res.pSubpasses(subpass)
      res

    renderPass = create(vkCreateRenderPass(device, renderPassInfo, null, _: LongBuffer))
  end createRenderPass

  def createGraphicsPipeline() =
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

    val vertShader = compileShader("09_shader_base.vert", shaderc_vertex_shader)
    val fragShader = compileShader("09_shader_base.frag", shaderc_fragment_shader)
    val vertShaderModule = createShaderModule(vertShader)
    val fragShaderModule = createShaderModule(fragShader)

    val shaderStages = VkPipelineShaderStageCreateInfo.callocStack(2, stack)
    populateShaderStage(shaderStages.get(0), VK_SHADER_STAGE_VERTEX_BIT, vertShaderModule)
    populateShaderStage(shaderStages.get(1), VK_SHADER_STAGE_FRAGMENT_BIT, fragShaderModule)

    // Fixed-Function Stages
    val vertexInputInfo: VkPipelineVertexInputStateCreateInfo =
      val info = VkPipelineVertexInputStateCreateInfo.calloc(stack)
      info.sType(VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
      info.pVertexBindingDescriptions(null)
      info.pVertexAttributeDescriptions(null)
      info

    val inputAssembly: VkPipelineInputAssemblyStateCreateInfo =
      val info = VkPipelineInputAssemblyStateCreateInfo.calloc(stack)
      info.sType(VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
      info.topology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
      info.primitiveRestartEnable(false)
      info

    val viewport: VkViewport.Buffer =
      val res = VkViewport.calloc(1, stack)
      res.x(0.0f)
      res.y(0.0f)
      res.width(swapChainExtent.width.toFloat)
      res.height(swapChainExtent.height.toFloat)
      res.minDepth(0.0f)
      res.maxDepth(1.0f)
      res

    val scissor: VkRect2D.Buffer =
      val res = VkRect2D.calloc(1, stack)
      res.offset(VkOffset2D.calloc(stack).set(0, 0))
      res.extent(swapChainExtent)
      res

    val viewportState: VkPipelineViewportStateCreateInfo =
      val res = VkPipelineViewportStateCreateInfo.calloc(stack)
      res.sType(VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
      res.pViewports(viewport)
      res.pScissors(scissor)
      res

    val rasterizer: VkPipelineRasterizationStateCreateInfo =
      val res = VkPipelineRasterizationStateCreateInfo.calloc(stack)
      res.sType(VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
      res.depthClampEnable(false)
      res.rasterizerDiscardEnable(false)
      res.polygonMode(VK_POLYGON_MODE_FILL)
      res.lineWidth(1.0f)
      res.cullMode(VK_CULL_MODE_BACK_BIT)
      res.frontFace(VK_FRONT_FACE_CLOCKWISE)
      res.depthBiasEnable(false)
      res.depthBiasConstantFactor(0.0f)
      res.depthBiasClamp(0.0f)
      res.depthBiasSlopeFactor(0.0f)
      res

    val multisampling: VkPipelineMultisampleStateCreateInfo =
      val res = VkPipelineMultisampleStateCreateInfo.calloc(stack)
      res.sType(VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
      res.sampleShadingEnable(false)
      res.rasterizationSamples(VK_SAMPLE_COUNT_1_BIT)
      res.minSampleShading(1.0f) // Optional
      res.pSampleMask(null) // Optional
      res.alphaToCoverageEnable(false) // Optional
      res.alphaToOneEnable(false) // Optional
      res

    val colorBlendAttachment: VkPipelineColorBlendAttachmentState.Buffer =
      val res = VkPipelineColorBlendAttachmentState.calloc(1, stack)
      res.colorWriteMask(VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT)
      res.blendEnable(false)
      res

    val colorBlending: VkPipelineColorBlendStateCreateInfo =
      val res = VkPipelineColorBlendStateCreateInfo.calloc(stack)
      res.sType(VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
      res.logicOpEnable(false)
      res.logicOp(VK_LOGIC_OP_COPY) // Optional
      res.pAttachments(colorBlendAttachment)
      res.blendConstants(stack.floats(0.0f, 0.0f, 0.0f, 0.0f))
      res

    pipelineLayout =
      val info = VkPipelineLayoutCreateInfo.calloc(stack)
      info.sType(VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
      create(vkCreatePipelineLayout(device, info, null, _: LongBuffer))

    // Clean-up
    vkDestroyShaderModule(device, fragShaderModule, null)
    vkDestroyShaderModule(device, vertShaderModule, null)
    shaderc_result_release(vertShader)
    shaderc_result_release(fragShader)
  end createGraphicsPipeline


  createInstance()
  if enableValidationLayers then
    debugMessenger = create(vkCreateDebugUtilsMessengerEXT(instance, debugMessengerCreateInfo, null, _: LongBuffer))
  surface = create(glfwCreateWindowSurface(instance, window, null, _: LongBuffer))
  pickPhysicalDevice()
  createLogicalDevice()
  createSwapChain()
  createImageViews()
  createRenderPass()
  createGraphicsPipeline()
}

def loop() =
  while !glfwWindowShouldClose(window) do
    glfwPollEvents()

def cleanup() =
  vkDestroyPipelineLayout(device, pipelineLayout, null)
  vkDestroyRenderPass(device, renderPass, null)
  for imageView <- swapChainImageViews do
    vkDestroyImageView(device, imageView, null)
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
  def apply(size: Int)(using MemoryStack): T

def alloc[T](size: Int = 1)(using stk: MemoryStack, tcl: Allocatable[T]) = tcl(size)

object Allocatable:
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

type Buf[T] = { def get(i: Int): T }
def create[T, Ptr <: Buf[T]: Allocatable](function: Ptr => Int)(using MemoryStack): T =
  val ptr: Ptr = alloc()
  if function(ptr) != VK_SUCCESS then
    throw RuntimeException(s"Failed to create a Vulkan object")
  ptr.get(0)

def querySeq[T, TgtBuf: AsScalaList[T, _]: Allocatable](function: (IntBuffer, TgtBuf | Null) => Int | Unit)(using MemoryStack) =
  val count: IntBuffer = alloc()
  function(count, null)
  val targetBuf: TgtBuf = alloc(count.get(0))
  function(count, targetBuf)
  targetBuf.toList

def query[T, Ptr <: Buf[T]: Allocatable](function: Ptr => Int | Unit)(using MemoryStack) =
  val ptr: Ptr = alloc()
  function(ptr)
  ptr.get(0)

def queryStruct[T: Allocatable](function: T => Int | Unit)(using MemoryStack) =
  val struct: T = alloc()
  function(struct)
  struct
