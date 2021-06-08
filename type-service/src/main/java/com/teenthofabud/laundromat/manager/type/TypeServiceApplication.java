package com.teenthofabud.laundromat.manager.type;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;

@SpringBootApplication
@EnableEurekaClient
public class TypeServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(TypeServiceApplication.class, args);
    }

}
