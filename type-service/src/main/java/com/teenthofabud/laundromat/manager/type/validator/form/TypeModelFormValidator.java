package com.teenthofabud.laundromat.manager.type.validator.form;

import com.teenthofabud.laundromat.manager.type.model.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.form.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.model.form.TypeModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TypeModelFormValidator implements Validator {

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
        }if(StringUtils.isEmpty(form.getName())) {
            log.debug("TypeModelForm.name is empty");
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
