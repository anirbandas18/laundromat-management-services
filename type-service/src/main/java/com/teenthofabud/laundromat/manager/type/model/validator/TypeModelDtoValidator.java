package com.teenthofabud.laundromat.manager.type.model.validator;

import com.teenthofabud.laundromat.manager.type.model.data.TypeModelDto;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class TypeModelDtoValidator implements Validator {

    @Autowired
    public void setTypeLOVValidator(TypeLOVValidator typeLOVValidator) {
        this.typeLOVValidator = typeLOVValidator;
    }

    private TypeLOVValidator typeLOVValidator;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeModelDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeModelDto dto = (TypeModelDto) target;
        Optional<String> optTypeLovId = dto.getTypeLovId();
        if((optTypeLovId.isPresent())) {
            boolean isValid = true;
            if(optTypeLovId.get() == null) {
                isValid = false;
                log.debug("TypeModelDto.typeLovId is null");
            } else {
                try {
                    Long typeLovId = Long.parseLong(optTypeLovId.get());
                    if(typeLovId <= 0L) {
                        isValid = false;
                        log.debug("TypeModelDto.typeLovId is invalid: discountModelId <= 0");
                    } else {
                        Errors internalError = new DirectFieldBindingResult(typeLovId, "typeLovId");
                        typeLOVValidator.validate(typeLovId, internalError);
                        if(internalError.hasGlobalErrors()) {
                            log.debug("TypeModelForm.typeLovId: corresponding {} is invalid", internalError.getGlobalError().getCode());
                            errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                            return;
                        }
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                    log.error("TypeModelDto.typeLovId is invalid", e);
                    log.debug("TypeModelDto.typeLovId is invalid");
                }
            }
            if(!isValid) {
                errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            }
            return;
        }
        Optional<String> optName = dto.getName();
        if((optName.isPresent()) && StringUtils.isEmpty(optName.get())) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeModelDto.name is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(!optActive.isPresent() || (optActive.isPresent() && StringUtils.isEmpty(optActive.get()))) {
            errors.rejectValue("active", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeModelDto.active is invalid");
            return;
        } else {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeModelDto.active is invalid");
                return;
            }
        }
    }
}
