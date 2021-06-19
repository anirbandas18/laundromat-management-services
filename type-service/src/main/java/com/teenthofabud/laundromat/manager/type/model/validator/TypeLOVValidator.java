package com.teenthofabud.laundromat.manager.type.model.validator;

import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TypeLOVValidator implements Validator {

    @Autowired
    public void setTypeLOVService(TypeLOVService typeLOVService) {
        this.typeLOVService = typeLOVService;
    }

    private TypeLOVService typeLOVService;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeLOVVo.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        Long typeLOVId = (Long) target;
        try {
            TypeLOVVo typeLOVVo = typeLOVService.retrieveDetailsById(typeLOVId);
            if(typeLOVVo.getId() == null || typeLOVVo.getId() <= 0L) {
                errors.reject("id", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeLOV.id: {} is invalid", typeLOVVo.getId());
                return;
            }
            if(StringUtils.isEmpty(typeLOVVo.getName())) {
                errors.reject("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeLOV.name: {} is invalid", typeLOVVo.getName());
                return;
            }
            if(!typeLOVVo.getActive()) {
                errors.reject("active", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeLOV is inactive");
                return;
            }
        } catch (TypeLOVException e) {
            log.error("typeLovId: {} not found", typeLOVId, e);
            errors.reject("typeLovId", TypeErrorCode.TYPE_NOT_FOUND.name());
            return;
        }
    }
}
