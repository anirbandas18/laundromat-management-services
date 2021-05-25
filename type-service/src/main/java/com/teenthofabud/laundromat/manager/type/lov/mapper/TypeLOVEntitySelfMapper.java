package com.teenthofabud.laundromat.manager.type.lov.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TypeLOVEntitySelfMapper implements SingleChannelMapper<TypeLOVEntity> {
    @Override
    public Optional<TypeLOVEntity> compareAndMap(TypeLOVEntity source, TypeLOVEntity target) {
        TypeLOVEntity expectedEntity = new TypeLOVEntity();
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TypeLOVEntity.id is valid");
        }
        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TypeLOVEntity.description is valid");
        }
        if(source.getName() != null && StringUtils.hasText(source.getName()) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source TypeLOVEntity.name is valid");
        }
        if(changeSW) {
            log.debug("All provided TypeLOVEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TypeLOVEntity attributes are valid");
            return Optional.empty();
        }
    }
}
