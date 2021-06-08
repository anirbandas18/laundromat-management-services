package com.teenthofabud.laundromat.manager.type.lov.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class TypeLOVFormRelaxedValidator implements RelaxedValidator<TypeLOVForm>  {
    @Override
    public Boolean validateLoosely(TypeLOVForm form, Errors errors) {
        if(form.getName() != null && form.getName().length() == 0) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeLOVForm.name is empty");
            return false;
        }
        log.debug("TypeLOVForm.name is valid");
        return true;
    }
}
