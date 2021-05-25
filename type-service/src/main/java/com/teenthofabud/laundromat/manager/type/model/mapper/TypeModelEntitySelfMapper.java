package com.teenthofabud.laundromat.manager.type.model.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TypeModelEntitySelfMapper implements SingleChannelMapper<TypeModelEntity> {
    @Override
    public Optional<TypeModelEntity> compareAndMap(TypeModelEntity source, TypeModelEntity target) {
        TypeModelEntity expectedEntity = new TypeModelEntity();
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TypeModelEntity.id is valid");
        }
        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TypeModelEntity.description is valid");
        }
        if(source.getName() != null && StringUtils.hasText(source.getName()) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source TypeModelEntity.name is valid");
        }
        if(changeSW) {
            log.debug("All provided TypeModelEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TypeModelEntity attributes are valid");
            return Optional.empty();
        }
    }
}
