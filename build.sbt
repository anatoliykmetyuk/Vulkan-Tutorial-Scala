val scala3Version = "3.1.2"
val lwjglVersion = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "vulkan-playground",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    javaOptions += "-XstartOnFirstThread",
    fork := true,

    libraryDependencies ++= List(
      "org.lwjgl" % "lwjgl"        % lwjglVersion,
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion,
      "org.lwjgl" % "lwjgl-openal" % lwjglVersion,
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion,
      "org.lwjgl" % "lwjgl-vulkan" % lwjglVersion,
      "org.lwjgl" % "lwjgl"        % lwjglVersion classifier "natives-macos",
      "org.lwjgl" % "lwjgl-assimp" % lwjglVersion classifier "natives-macos",
      "org.lwjgl" % "lwjgl-glfw"   % lwjglVersion classifier "natives-macos",
      "org.lwjgl" % "lwjgl-openal" % lwjglVersion classifier "natives-macos",
      "org.lwjgl" % "lwjgl-stb"    % lwjglVersion classifier "natives-macos",
      "org.lwjgl" % "lwjgl-vulkan" % lwjglVersion classifier "natives-macos",
    ),

    libraryDependencies += "org.joml" % "joml" % "1.10.4",
  )
