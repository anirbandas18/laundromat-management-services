package com.teenthofabud.laundromat.manager.access.permission.converter;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PermissionEntity2VoConverter implements Converter<PermissionEntity, PermissionVo> {
    @Override
    public PermissionVo convert(PermissionEntity entity) {
        PermissionVo vo = new PermissionVo();
        TypeModelVo resource = new TypeModelVo();
        resource.setName(entity.getResource().getName());
        resource.setId(entity.getResource().getId());
        vo.setResource(resource);
        TypeModelVo operation = new TypeModelVo();
        operation.setName(entity.getOperation().getName());
        operation.setId(entity.getOperation().getId());
        vo.setOperation(operation);
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
