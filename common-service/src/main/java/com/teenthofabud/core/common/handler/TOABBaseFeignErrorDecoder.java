package com.teenthofabud.core.common.handler;

import com.teenthofabud.core.common.data.error.TOABBaseException;
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

    private static Map<String, TOABBaseFeignExceptionHandler> FEIGN_CLIENT_EXCEPTION_HANDLERS;
    private static ErrorDecoder.Default DEFAULT_FEIGN_ERROR_DECODER;

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
        DEFAULT_FEIGN_ERROR_DECODER = new Default();
        FEIGN_CLIENT_EXCEPTION_HANDLERS = new TreeMap<>();
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
                TOABBaseFeignExceptionHandler feignClientExceptionHandler = applicationContext.getBean(errorMarker.value());
                FEIGN_CLIENT_EXCEPTION_HANDLERS.put(configKey, feignClientExceptionHandler);
            }
        }
    }

    @Override
    public Exception decode(String s, Response response) {
        TOABBaseFeignExceptionHandler feignClientExceptionHandler = FEIGN_CLIENT_EXCEPTION_HANDLERS.get(s);
        if (feignClientExceptionHandler != null) {
            Optional<TOABBaseException> optionalTOABBaseException = feignClientExceptionHandler.parseResponseToException(response);
            if(optionalTOABBaseException.isPresent()) {
                return optionalTOABBaseException.get();
            }
        }
        return DEFAULT_FEIGN_ERROR_DECODER.decode(s, response);
    }
}
