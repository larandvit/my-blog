Title: Get Started with Custom Connector Plugin in Trino
Date: 2023-03-05
Category: Trino
Cover: /extra/trino-logo.png

If you need to create a custom connector plugin in [Trino](https://trino.io/), the best way to get started is to pick up an existing connector. Trino includes a comprehensive list of native connectors of any flavor. Separation of code from the main repository is requesting some modifications to it. Those changes are easier than developing the code from scratch. Your connector inherited from Trino can be smoothly integrated back to Trino. This is the key to consider in your development.

The sample is created in CentOS 7 for Trino 393. As a Trino prototype connector is taken `jmx` one.

##1. Copy code from plugins folder

  Trino `jmx` plugin is located in [plugin](https://github.com/trinodb/trino/tree/master/plugin/trino-jmx) folder of [Trino](https://github.com/trinodb/trino) GitHub repository. It is a subproject of the Trino project. Copy content of `trino-jmx` folder to your project folder, for example, `custom-jmx` one. There are `src` folder and `pom.xml` file.

##2. Remove parent section from pom.xml

  Parent section refers to the Trino project and it does not need for your custom plugin.

    :::xml
    <parent>
        <groupId>io.trino</groupId>
        <artifactId>trino-root</artifactId>
        <version>410-SNAPSHOT</version>
        <relativePath>../../pom.xml</relativePath>
    </parent>
  
##3. Add root elements

  Those elements describe your plugin, and they usually go at very beginning. Replace those elements with your values. 

    :::xml
    <version>393</version>
    <groupId>com.example.trino.plugin</groupId>
    <artifactId>custom-jmx</artifactId>
    <description>Trino Custom JMX Connector</description>

##4. Add properties section

  The elements are required by Maven compiler.

    :::xml
    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
    </properties>

##5. Remove unused variables

    :::xml
    <properties>
        <air.main.basedir>${project.parent.basedir}</air.main.basedir>
    </properties>

##6. Add version to each dependency

  Parent pom.xml file defines versions of each dependency. Those dependencies should be added to the `jmx` project. Mostly of versions can be found in the parent `pom.xml` file, but some of them are not in the parent file, and you can download [.tar.gz Trino installation package](https://trino.io/download.html), for example, 393 release direct link [https://repo1.maven.org/maven2/io/trino/trino-server/393/trino-server-393.tar.gz](https://repo1.maven.org/maven2/io/trino/trino-server/393/trino-server-393.tar.gz). Versions are there if you open `plugin/jmx` folder.

  Original dependency

    :::xml
    <dependency>
        <groupId>io.airlift</groupId>
        <artifactId>units</artifactId>
    </dependency>

  Adjusted dependency

    :::xml
    <dependency>
        <groupId>io.airlift</groupId>
        <artifactId>units</artifactId>
        <version>1.6</version>
    </dependency>

  Also, you can add a variable to store version.

  Define the variable in `properties` section.

    :::xml
    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <dep.airlift.version>218</dep.airlift.version>
    </properties>

  Use the variable in the file

    :::xml
    <dependency>
        <groupId>io.airlift</groupId>
        <artifactId>concurrent</artifactId>
        <version>${dep.airlift.version}</version>
    </dependency>

  If you miss any versions, the compiler will notify and you can address it.

##7. Add io.trino.spi.Plugin file

  This file is requested by SPI. The location is `src\main\resources\META-INF\services`. The name is `io.trino.spi.Plugin`.

    :::java
    com.example.trino.plugin.custom.JmxPlugin

##8. Modify location of java file to match your package

  Make structure of folders in `src/java` as `com/example/trino/custom`.

##8. Replace package in each file

  The folder is `src/java/com/example/trino/custom`. Make changes to each java file.

    :::java
    package com.example.trino.plugin.custom;
  
##9. Remove checkSpiVersion line from Factory file

  It is a validation of Trino plugin used in Trino. A custom plugin does not have it. The file path is `src/java/com/example/trino/custom/JmxConnectorFactory.java`.

    :::java
    @Override
    public Connector create(String catalogName, Map<String, String> config, ConnectorContext context)
    {
        checkSpiVersion(context, this);

        Bootstrap app = new Bootstrap( 

##10. Define connector name in Factory file

  The file path is `src/java/com/example/trino/custom/JmxConnectorFactory.java`.

    :::java
    @Override
    public String getName()
    {
        return "customjmx";
    }

##11. Create development environment

  * Validate that java version is 17

        :::bash
        java --version

  * Validate that minimum Maven version is 3.6.0

        :::bash
        mvn --version

  * Unpack the maven-wrapper distribution files to the current project source tree

        :::bash
        mvn wrapper:wrapper

##12. Make a package

  This step might bring errors if there are any issues in pom.xml file or something else.

    :::bash
    ./mvnw clean package dependency:copy-dependencies -DskipTests -DincludeScope=compile

##13. Test connector

  * Copy files to Trino `plugin` folder. For example, if your folder is `/var/lib/trino/data/plugin`.

        :::bash
        sudo mkdir /var/lib/trino/data/plugin/custom-jmx
        sudo cp -v target/dependency/*.jar /var/lib/trino/data/plugin/custom-jmx/
        sudo cp -v target/*.jar /var/lib/trino/data/plugin/custom-jmx/

  * Create catalog properties file

        :::ini
        connector.name=customjmx

  * Restart Trino cluster

  * Find a new catalog with `customjmx` name.

## Resources
* [Trino GitHub](https://github.com/trinodb/trino)
