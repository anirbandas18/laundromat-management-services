package com.teenthofabud.core.common.marker;

import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface TOABFeignErrorHandler {

    Class<? extends TOABFeignBaseExceptionHandler> value();

}
