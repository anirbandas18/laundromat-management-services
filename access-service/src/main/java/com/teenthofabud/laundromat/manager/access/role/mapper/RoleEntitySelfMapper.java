package com.teenthofabud.laundromat.manager.access.role.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class RoleEntitySelfMapper implements SingleChannelMapper<RoleEntity> {
    @Override
    public Optional<RoleEntity> compareAndMap(RoleEntity source, RoleEntity target) {
        RoleEntity expectedEntity = new RoleEntity();
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source RoleEntity.id is valid");
        }
        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source RoleEntity.description is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source RoleEntity.name is valid");
        }
        if(changeSW) {
            log.debug("All provided RoleEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided RoleEntity attributes are valid");
            return Optional.empty();
        }
    }
}
