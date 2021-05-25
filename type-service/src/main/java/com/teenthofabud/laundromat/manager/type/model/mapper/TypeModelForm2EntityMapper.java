package com.teenthofabud.laundromat.manager.type.model.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.error.TypeException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TypeModelForm2EntityMapper implements DualChannelMapper<TypeModelEntity, TypeModelForm> {

    @Autowired
    public void setTypeLOVRepository(TypeLOVRepository typeLOVRepository) {
        this.typeLOVRepository = typeLOVRepository;
    }

    private TypeLOVRepository typeLOVRepository;

    @Override
    public Optional<TypeModelEntity> compareAndMap(TypeModelEntity actualEntity, TypeModelForm form) throws TOABBaseException {
        TypeModelEntity expectedEntity = new TypeModelEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TypeModelEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TypeModelEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TypeModelEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(form.getName()) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TypeModelForm.name: {} is different as TypeModelEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            changeSW = true;
            log.debug("TypeModelForm.name: {} is same to TypeModelEntity.name: {}", form.getName(), actualEntity.getName());
        }
        if(StringUtils.hasText(form.getDescription()) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TypeModelForm.description: {} is different as TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            changeSW = true;
            log.debug("TypeModelForm.description: {} is same to TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        }
        if(form.getTypeLovId() != null &&
                form.getTypeLovId() != actualEntity.getTypeLov().getId()) {
            if(typeLOVRepository.existsById(form.getTypeLovId())) {
                Optional<TypeLOVEntity> optTypeLOVEntity = typeLOVRepository.findById(form.getTypeLovId());
                TypeLOVEntity typeLOVEntity = optTypeLOVEntity.get();
                if(typeLOVEntity.getActive()) {
                    expectedEntity.setTypeLov(typeLOVEntity);
                    changeSW = true;
                    log.debug("TypeModelForm.typeLovId: {} is different as TypeModelEntity.typeLovId: {}", form.getDescription(), actualEntity.getDescription());
                } else {
                    throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_INACTIVE, new Object [] { "typeLovId", String.valueOf(form.getTypeLovId()) });
                }
            } else {
                throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_NOT_FOUND, new Object [] { "typeLovId", String.valueOf(form.getTypeLovId()) });
            }
        } else {
            expectedEntity.setTypeLov(actualEntity.getTypeLov());
            changeSW = true;
            log.debug("TypeModelForm.description: {} is same to TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
