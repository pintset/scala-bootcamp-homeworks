## 06. sbt

Define an auto plugin `BulkySourcesPlugin` inside fork of `scala-bootcamp` or inside some other project. The plugin should contain:
    setting `bulkyThresholdInLines` of type `SettingKey[Int]` with default value = 100
    task    `bulkySources` of type `TaskKey[Seq[(Int, File)]]`

Implement `bulkySources` for main and test sources so `show bulkySources` outputs a list of "large" source files, where "large" is defined as a number of lines in the source file and can be set with `bulkyThresholdInLines` option. The output should be descendingly sorted (larger files before smaller ones)

For example, `show bulkySources` may print out

    ```
    [info] * (130, .../src/main/scala/../../../A.scala)
    [info] * (100, .../src/main/scala/../../../B.scala)
    ```
And `show test:bulkySources` may print out

    ```
    [info] * (170, .../src/test/scala/../../../Y.scala)
    [info] * (110, .../src/test/scala/../../../Z.scala)
    ```

### Optional task 1
Implement the plugin as a separate project

### Optional task 2
Implement `bulkySources` as `InputTask` (https://www.scala-sbt.org/1.x/docs/Input-Tasks.html) which uses `bulkyThresholdInLines` parameter in case parameters list is empty (by default) or uses a value provided by a user  

How to create auto plugins:
    https://github.com/jsuereth/sbt-in-action-examples/tree/master/chapter11
    https://livebook.manning.com/book/sbt-in-action/chapter-11
    https://www.scala-sbt.org/1.x/docs/Plugins.html#Creating+an+auto+plugin
    https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html


