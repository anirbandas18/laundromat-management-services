package com.teenthofabud.core.common.proxy;

import com.teenthofabud.core.common.error.TOABFeignException;
import feign.Response;

import java.util.Optional;

public interface TOABFeignBaseExceptionHandler {

    public Optional<? extends TOABFeignException> parseResponseToException(Response response);

}
