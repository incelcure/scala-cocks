FROM openjdk:17

WORKDIR /app

COPY target/scala-2.13/cocks-service.jar /app/app.jar

EXPOSE 8080

CMD ["java", "-jar", "/app/app.jar"]
