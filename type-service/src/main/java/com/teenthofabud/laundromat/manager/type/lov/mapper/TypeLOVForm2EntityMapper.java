package com.teenthofabud.laundromat.manager.type.lov.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TypeLOVForm2EntityMapper implements DualChannelMapper<TypeLOVEntity, TypeLOVForm> {
    @Override
    public Optional<TypeLOVEntity> compareAndMap(TypeLOVEntity actualEntity, TypeLOVForm form) {
        TypeLOVEntity expectedEntity = new TypeLOVEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TypeLOVEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TypeLOVEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TypeLOVEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(form.getName()) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TypeLOVForm.name: {} is different as TypeLOVEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            //changeSW = true;
            log.debug("TypeLOVForm.name: {} is same to TypeLOVEntity.name: {}", form.getName(), actualEntity.getName());
        }
        if(StringUtils.hasText(form.getDescription()) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TypeLOVForm.description: {} is different as TypeLOVEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            //changeSW = true;
            log.debug("TypeLOVForm.description: {} is same to TypeLOVEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
