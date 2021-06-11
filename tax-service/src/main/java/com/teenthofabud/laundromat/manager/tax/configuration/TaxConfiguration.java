package com.teenthofabud.laundromat.manager.tax.configuration;

import com.teenthofabud.core.common.factory.TOABFeignErrorDecoderFactory;
import feign.codec.ErrorDecoder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableFeignClients(basePackages = "com.teenthofabud.laundromat.manager.type.proxy")
@EnableEurekaClient
public class TaxConfiguration {

    @Autowired
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    private ApplicationContext applicationContext;

    @Bean
    public ErrorDecoder errorDecoder() {
        return new TOABFeignErrorDecoderFactory(applicationContext, "com.teenthofabud.laundromat.manager.type.proxy");
    }

}
