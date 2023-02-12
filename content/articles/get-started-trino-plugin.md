Title: Get Started with Trino Plugin
Date: 2023-01-30
Category: Trino
Cover: /extra/trino-logo.png

Trino is fully customizable with custom plugins. Those plugins can be developed in Java and deployed to Trino clusters. One of the aspects of success in development Trino plugins depends on your development environment. If you are able to mimic the Trino team environment, you might encounter less issues and problems during development and debug stages. The article shows some mandatory or preferable steps before starting your plugin.

The sample is created in CentOS 7 for Trino 393.

##1. Java 17 is supported and the minimum version is 17.0.3.

    :::bash
    java --version

##2. Maven is installed. Minimal Maven version is 3.6.0 for Java 17 as per Maven documentation.

    :::bash
    mvn --version

##3. Generate a new plugin project in Maven

    :::bash
    mvn archetype:generate -DgroupId=com.sample.plugin -DartifactId=sample-plugin -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false

  The command creates a new subfolder with `sample-plugin` name. The subfolder includes `pom.xml` file and children folders as per `groupId=com.sample.plugin` parameter.

##4. Go to the new folder

    :::bash
    cd sample-plugin

##5. Modify pom file

    :::xml
    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      <modelVersion>4.0.0</modelVersion>

      <groupId>com.sample.plugin</groupId>
      <artifactId>sample-plugin</artifactId>
      <version>393</version>
      <description>Trino - Sample Plugin</description>
      
      <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
      </properties>

      <dependencies>
        <!-- Trino mandatory dependency -->
        <dependency>
          <groupId>io.trino</groupId>
          <artifactId>trino-spi</artifactId>
          <version>${project.version}</version>
          <scope>provided</scope>
        </dependency>

        <!-- Add your dependencies -->    

        <!-- Quick start archetype test if needed-->
        <dependency>
          <groupId>junit</groupId>
          <artifactId>junit</artifactId>
          <version>3.8.1</version>
          <scope>test</scope>
        </dependency>
      </dependencies>
    </project>

##6. Unpack the maven-wrapper distribution files to the current project source tree

    :::bash
    mvn wrapper:wrapper

  The Maven Wrapper makes life easier to ensure that your Maven build has everything necessary to run your Maven build.
  It is added `.mvn` subfolder, and `mvnw` and `mvnw.cmd` files.

##7. Create the package

    :::bash
    ./mvnw clean package dependency:copy-dependencies

  Instead of `mvn` command, the Maven wrapper is used.

##8. Test

    :::bash
    java -cp target/sample-plugin-393.jar com.sample.plugin.App

  This test is not plugin test. It's just test to validate the development environment.

##9. Clean up the project from the sample

  * Delete sample code
  * Delete tests
  * Remove junit dependency from pom file

##10. Add your plugin code

## Resources
* [Trino GitHub](https://github.com/trinodb/trino)
* [Maven in 5 Minutes](https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html)
* [Wrapper Plugin Documentation](https://maven.apache.org/wrapper/maven-wrapper-plugin/plugin-info.html)
* [Introduction to Archetypes](https://maven.apache.org/guides/introduction/introduction-to-archetypes.html)
* [Project creation](https://maven.apache.org/archetype/maven-archetype-plugin/usage.html)
