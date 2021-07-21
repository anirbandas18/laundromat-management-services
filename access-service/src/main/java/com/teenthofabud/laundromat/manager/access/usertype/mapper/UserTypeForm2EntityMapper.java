package com.teenthofabud.laundromat.manager.access.usertype.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class UserTypeForm2EntityMapper implements DualChannelMapper<UserTypeEntity, UserTypeForm> {
    @Override
    public Optional<UserTypeEntity> compareAndMap(UserTypeEntity actualEntity, UserTypeForm form) {
        UserTypeEntity expectedEntity = new UserTypeEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying UserTypeEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying UserTypeEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying UserTypeEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("UserTypeForm.name: {} is different as UserTypeEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("UserTypeForm.name: is unchanged");
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription())) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("UserTypeForm.description: {} is different as UserTypeEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("UserTypeForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
