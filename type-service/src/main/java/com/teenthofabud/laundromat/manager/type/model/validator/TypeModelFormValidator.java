package com.teenthofabud.laundromat.manager.type.model.validator;

import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TypeModelFormValidator implements Validator {

    @Autowired
    public void setTypeLOVValidator(TypeLOVValidator typeLOVValidator) {
        this.typeLOVValidator = typeLOVValidator;
    }

    private TypeLOVValidator typeLOVValidator;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeModelForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeModelForm form = (TypeModelForm) target;
        if(form.getTypeLovId() == null || form.getTypeLovId() <= 0L) {
            log.debug("TypeModelForm.typeLovId is invalid");
            errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
        if(form.getTypeLovId() != null && form.getTypeLovId() > 0L) {
            Errors internalError = new DirectFieldBindingResult(form.getTypeLovId(), "typeLovId");
            typeLOVValidator.validate(form.getTypeLovId(), internalError);
            if(internalError.hasGlobalErrors()) {
                log.debug("TypeModelForm.typeLovId: corresponding {} is invalid", internalError.getGlobalError().getCode());
                errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(StringUtils.isEmpty(form.getName())) {
            log.debug("TypeModelForm.name is empty");
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
