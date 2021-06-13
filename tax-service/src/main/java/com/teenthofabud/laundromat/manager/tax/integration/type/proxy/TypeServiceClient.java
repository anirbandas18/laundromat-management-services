package com.teenthofabud.laundromat.manager.tax.integration.type.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.laundromat.manager.tax.integration.type.error.TypeException;
import com.teenthofabud.laundromat.manager.tax.integration.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.tax.integration.type.error.TypeServiceClientExceptionHandler;
import com.teenthofabud.laundromat.manager.tax.integration.type.proxy.impl.TypeServiceClientFallbackImpl;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "type-service", url = "${lms.tax.type-service.url}", fallback = TypeServiceClientFallbackImpl.class)
public interface TypeServiceClient {

    @GetMapping("/model/{id}")
    @TOABFeignErrorHandler(TypeServiceClientExceptionHandler.class)
    public TypeModelVo getTypeModelDetailsById(@PathVariable Long id) throws TypeException;

}