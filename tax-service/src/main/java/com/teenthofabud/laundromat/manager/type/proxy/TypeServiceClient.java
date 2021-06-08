package com.teenthofabud.laundromat.manager.type.proxy;

import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.proxy.fallback.TypeServiceClientFallback;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;


@FeignClient(value = "type-service", url = "${lms.tax.type-service.url}", fallback = TypeServiceClientFallback.class)
public interface TypeServiceClient {

    @GetMapping("/typemodel/{id}")
    public TypeModelVo getTypeModelDetailsById(@PathVariable Long id) throws TypeException;

}
