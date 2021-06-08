package com.teenthofabud.core.common.handler;

import com.teenthofabud.core.common.data.error.TOABFeignException;
import com.teenthofabud.core.common.proxy.TOABBaseFeignExceptionHandler;
import com.teenthofabud.core.common.proxy.TOABFeignErrorHandler;
import feign.Feign;
import feign.Response;
import feign.codec.ErrorDecoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;

import javax.annotation.PostConstruct;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

@Component
@Slf4j
public class TOABBaseFeignErrorDecoder implements ErrorDecoder {

    private Map<String, TOABBaseFeignExceptionHandler/*<? extends TOABFeignException>*/> feignClientExceptionHandlers;
    private ErrorDecoder.Default defaultFeignErrorDecoder;

    @Autowired
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }
    private ApplicationContext applicationContext;

    private TOABFeignErrorHandler getFeignErrorHandlingMarker(Method m) {
        TOABFeignErrorHandler errorMarker = m.getAnnotation(TOABFeignErrorHandler.class);
        if (errorMarker == null) {
            errorMarker = m.getDeclaringClass().getAnnotation(TOABFeignErrorHandler.class);
        }
        return errorMarker;
    }

    @PostConstruct
    private void init() {
        defaultFeignErrorDecoder = new Default();
        feignClientExceptionHandlers = new TreeMap<>();
        Map<String, Object> feignClients = applicationContext.getBeansWithAnnotation(FeignClient.class);
        List<Method> clientMethods = feignClients.values().stream()
                .map(Object::getClass)
                .map(aClass -> aClass.getInterfaces()[0])// Dynamic implementations of Feign clients can include JDK proxies for creation, hence we need to call the class' getInterfaces() method and use its first value of the returned collection [0] to get the actual interface with its methods.
                .map(ReflectionUtils::getDeclaredMethods)
                .flatMap(Arrays::stream)
                .collect(Collectors.toList());
        for (Method m : clientMethods) {
            String configKey = Feign.configKey(m.getDeclaringClass(), m);
            TOABFeignErrorHandler errorMarker = getFeignErrorHandlingMarker(m);
            if (errorMarker != null) {
                TOABBaseFeignExceptionHandler/*<? extends TOABFeignException>*/ feignClientExceptionHandler = applicationContext.getBean(errorMarker.value());
                feignClientExceptionHandlers.put(configKey, feignClientExceptionHandler);
            }
        }
    }

    @Override
    public Exception decode(String s, Response response) {
        TOABBaseFeignExceptionHandler/*<? extends TOABFeignException>*/ feignClientExceptionHandler = feignClientExceptionHandlers.get(s);
        if (feignClientExceptionHandler != null) {
            Optional<? extends TOABFeignException> optionalTOABBaseException = feignClientExceptionHandler.parseResponseToException(response);
            if(optionalTOABBaseException.isPresent()) {
                return optionalTOABBaseException.get();
            }
        }
        return defaultFeignErrorDecoder.decode(s, response);
    }
}
