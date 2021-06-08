package com.teenthofabud.core.common.proxy;

import com.teenthofabud.core.common.data.error.TOABFeignException;
import feign.Response;

import java.util.Optional;

public interface TOABBaseFeignExceptionHandler<T extends TOABFeignException> {
    public Optional<T> parseResponseToException(Response response);

}
