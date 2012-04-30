set SCRIPT_DIR=%~dp0
java -Xmx2000m -Xms1000m -jar "%SCRIPT_DIR%sbt-launch.jar" %*
