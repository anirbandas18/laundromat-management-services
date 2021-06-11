package com.teenthofabud.core.common.factory;

import com.teenthofabud.core.common.error.TOABFeignException;
import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;
import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import feign.Feign;
import feign.Response;
import feign.codec.ErrorDecoder;
import lombok.extern.slf4j.Slf4j;
import org.reflections.Reflections;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class TOABFeignErrorDecoderFactory implements ErrorDecoder, InitializingBean {

    private Map<String, TOABFeignBaseExceptionHandler> feignClientExceptionHandlers;
    private ErrorDecoder.Default defaultFeignErrorDecoder;
    private String basePackageForFeign;
    private ApplicationContext applicationContext;

    @Autowired
    public TOABFeignErrorDecoderFactory(ApplicationContext applicationContext, String basePackageForFeign) {
        this.applicationContext = applicationContext;
        this.basePackageForFeign = basePackageForFeign;
    }

    private TOABFeignErrorHandler getFeignErrorHandlingMarker(Method m) {
        TOABFeignErrorHandler errorMarker = m.getAnnotation(TOABFeignErrorHandler.class);
        if (errorMarker == null) {
            errorMarker = m.getDeclaringClass().getAnnotation(TOABFeignErrorHandler.class);
        }
        return errorMarker;
    }

    @Override
    public Exception decode(String s, Response response) {
        TOABFeignBaseExceptionHandler feignClientExceptionHandler = feignClientExceptionHandlers.get(s);
        if (feignClientExceptionHandler != null) {
            Optional<? extends TOABFeignException> optionalTOABBaseException = feignClientExceptionHandler.parseResponseToException(response);
            if(optionalTOABBaseException.isPresent()) {
                return optionalTOABBaseException.get();
            }
        }
        return defaultFeignErrorDecoder.decode(s, response);
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        defaultFeignErrorDecoder = new Default();
        feignClientExceptionHandlers = new TreeMap<>();
        Reflections reflections = new Reflections(basePackageForFeign);
        List<Class<?>> feignClients = new ArrayList<>(reflections.getTypesAnnotatedWith(FeignClient.class));
        List<Method> clientMethods = feignClients.stream()
                .map(ReflectionUtils::getDeclaredMethods)
                .flatMap(Arrays::stream)
                .collect(Collectors.toList());
        for (Method m : clientMethods) {
            String configKey = Feign.configKey(m.getDeclaringClass(), m);
            TOABFeignErrorHandler errorMarker = getFeignErrorHandlingMarker(m);
            if (errorMarker != null) {
                TOABFeignBaseExceptionHandler feignClientExceptionHandler = applicationContext.getBean(errorMarker.value());
                feignClientExceptionHandlers.put(configKey, feignClientExceptionHandler);
            }
        }
    }

}
