package com.teenthofabud.laundromat.manager.tax.integration.type.proxy.impl;

import com.teenthofabud.laundromat.manager.tax.integration.type.proxy.TypeServiceClient;
import com.teenthofabud.laundromat.manager.tax.integration.type.data.TypeModelVo;
import org.springframework.stereotype.Component;

@Component
public class TypeServiceClientFallbackImpl implements TypeServiceClient {

    @Override
    public TypeModelVo getTypeModelDetailsById(Long id) {
        return new TypeModelVo();
    }
}
