package com.teenthofabud.laundromat.manager.type.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.error.TypeServiceClientExceptionHandler;
import com.teenthofabud.laundromat.manager.type.proxy.fallback.TypeServiceClientFallback;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "type-service", url = "${lms.tax.type-service.url}", fallback = TypeServiceClientFallback.class)
public interface TypeServiceClient {

    @GetMapping("/model/{id}")
    @TOABFeignErrorHandler(TypeServiceClientExceptionHandler.class)
    public TypeModelVo getTypeModelDetailsById(@PathVariable Long id) throws TypeException;

}