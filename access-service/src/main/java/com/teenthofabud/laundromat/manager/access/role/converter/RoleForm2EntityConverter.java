package com.teenthofabud.laundromat.manager.access.role.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class RoleForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<RoleForm, RoleEntity> {

    @Override
    public RoleEntity convert(RoleForm form) {
        RoleEntity entity = new RoleEntity();
        entity.setName(form.getName());
        entity.setDescription(form.getDescription());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
