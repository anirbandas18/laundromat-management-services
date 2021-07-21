package com.teenthofabud.laundromat.manager.access.userrole.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class UserRoleEntitySelfMapper implements SingleChannelMapper<UserRoleEntity> {
    @Override
    public Optional<UserRoleEntity> compareAndMap(UserRoleEntity source, UserRoleEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source UserRoleEntity.id is valid");
        }
        if(source.getUserType() != null && source.getUserType().getId() != null && source.getUserType().getId() > 0
                && !source.getUserType().getId().equals(target.getUserType().getId())) {
            target.setUserType(source.getUserType());
            changeSW = true;
            log.debug("Source UserRoleEntity.userType.id is valid");
        }
        if(source.getRole() != null && source.getRole().getId() != null && source.getRole().getId() > 0
                && !source.getRole().getId().equals(target.getRole().getId())) {
            target.setRole(source.getRole());
            changeSW = true;
            log.debug("Source UserRoleEntity.role.id is valid");
        }
        if(changeSW) {
            log.debug("All provided UserRoleEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided UserRoleEntity attributes are valid");
            return Optional.empty();
        }
    }
}
