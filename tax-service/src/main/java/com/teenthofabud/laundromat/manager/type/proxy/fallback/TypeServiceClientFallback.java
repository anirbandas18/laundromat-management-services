package com.teenthofabud.laundromat.manager.type.proxy.fallback;

import com.teenthofabud.laundromat.manager.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.proxy.TypeServiceClient;
import org.springframework.stereotype.Component;

@Component
public class TypeServiceClientFallback implements TypeServiceClient {

    @Override
    public TypeModelVo getTypeModelDetailsById(Long id) {
        return new TypeModelVo();
    }
}
