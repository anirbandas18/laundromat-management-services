package com.teenthofabud.laundromat.manager.access.resource.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ResourceForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<ResourceForm, ResourceEntity> {

    @Override
    public ResourceEntity convert(ResourceForm form) {
        ResourceEntity entity = new ResourceEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
