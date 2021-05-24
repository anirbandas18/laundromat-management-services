package com.teenthofabud.laundromat.manager.type.validator.form;

import com.teenthofabud.laundromat.manager.type.model.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.form.TypeLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TypeLOVFormValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeLOVForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeLOVForm form = (TypeLOVForm) target;
        if(StringUtils.isEmpty(form.getName())) {
            log.debug("TypeLOVForm.name is empty");
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
