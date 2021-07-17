package com.teenthofabud.laundromat.manager.access.permission.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class PermissionEntitySelfMapper implements SingleChannelMapper<PermissionEntity> {
    @Override
    public Optional<PermissionEntity> compareAndMap(PermissionEntity source, PermissionEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source PermissionEntity.id is valid");
        }
        if(source.getResource() != null && source.getResource().getId() != null && source.getResource().getId() > 0
                && !source.getResource().getId().equals(target.getResource().getId())) {
            target.setResource(source.getResource());
            changeSW = true;
            log.debug("Source PermissionEntity.resource.id is valid");
        }
        if(source.getOperation() != null && source.getOperation().getId() != null && source.getOperation().getId() > 0
                && !source.getOperation().getId().equals(target.getOperation().getId())) {
            target.setOperation(source.getOperation());
            changeSW = true;
            log.debug("Source PermissionEntity.operation.id is valid");
        }
        if(changeSW) {
            log.debug("All provided PermissionEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided PermissionEntity attributes are valid");
            return Optional.empty();
        }
    }
}
