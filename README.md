# billiard-system-3d

## How to use
## Option One: IntelliJ IDEA

- Download and install the [IntelliJ IDEA](https://www.jetbrains.com/idea/) ide
- Add the Scala and SBT Executor plugins to your ide. *(File -> Settings -> Plugins)*
- Open the `build.sbt` file as a project.
- Go to the main class or a worksheet and run them in IntelliJ

## Option Two: Scala build tool (sbt)

- Install sbt 1.4.7
- Clone this repo and navigate to source folder
- Use `sbt test` to run the unit tests
- Use `sbt clean assembly` to compile the code
- JAR file is will be located in `target\scala-2.13\` and will be named `billiard-system-3d-assembly-{versionNo}.jar`
- To execute the JAR file
  1. if Scala 2.13.4 is available: `scala path/to/jar/file.jar`
  2. if Java 8 or higher is available: `java -jar path/to/jar/file.jar`