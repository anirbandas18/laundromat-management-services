package com.teenthofabud.laundromat.manager.access.role.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class RoleForm2EntityMapper implements DualChannelMapper<RoleEntity, RoleForm> {
    @Override
    public Optional<RoleEntity> compareAndMap(RoleEntity actualEntity, RoleForm form) {
        RoleEntity expectedEntity = new RoleEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying RoleEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying RoleEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying RoleEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("RoleForm.name: {} is different as RoleEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("RoleForm.name: is unchanged");
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription())) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("RoleForm.description: {} is different as RoleEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("RoleForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
