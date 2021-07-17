package com.teenthofabud.laundromat.manager.access.resource.converter;

import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ResourceEntity2VoConverter implements Converter<ResourceEntity, ResourceVo> {
    @Override
    public ResourceVo convert(ResourceEntity entity) {
        ResourceVo vo = new ResourceVo();
        vo.setName(entity.getName());
        vo.setDescription(entity.getDescription());
        vo.setId(entity.getId());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
