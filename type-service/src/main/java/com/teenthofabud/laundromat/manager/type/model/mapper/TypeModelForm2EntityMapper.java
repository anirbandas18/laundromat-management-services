package com.teenthofabud.laundromat.manager.type.model.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.validator.TypeLOVValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.Optional;

@Component
@Slf4j
public class TypeModelForm2EntityMapper implements DualChannelMapper<TypeModelEntity, TypeModelForm> {

    @Autowired
    public void setTypeLOVValidator(TypeLOVValidator typeLOVValidator) {
        this.typeLOVValidator = typeLOVValidator;
    }

    private TypeLOVValidator typeLOVValidator;

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
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TypeModelForm.name: {} is different as TypeModelEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("TypeModelForm.name: is unchanged");
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription())) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TypeModelForm.description: {} is different as TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("TypeModelForm.description: is unchanged");
        }
        if(form.getTypeLovId() != null &&
                !form.getTypeLovId().equals(actualEntity.getTypeLov().getId())) {
            Errors internalError = new DirectFieldBindingResult(form.getTypeLovId(), "typeLovId");
            typeLOVValidator.validate(form.getTypeLovId(), internalError);
            if(internalError.hasGlobalErrors()) {
                log.debug("TypeModelForm.typeLovId: corresponding {} is invalid", internalError.getGlobalError().getCode());
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object [] { "typeLovId", String.valueOf(form.getTypeLovId()) });
            } else {
                Optional<TypeLOVEntity> optTypeLOVEntity = typeLOVRepository.findById(form.getTypeLovId());
                expectedEntity.setTypeLov(optTypeLOVEntity.get());
                changeSW = true;
                log.debug("TypeModelForm.typeLovId: {} is different as TypeModelEntity.typeLovId: {}", form.getTypeLovId(), actualEntity.getTypeLov().getId());
            }
        } else {
            expectedEntity.setTypeLov(actualEntity.getTypeLov());
            log.debug("TypeModelForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}