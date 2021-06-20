package com.teenthofabud.laundromat.manager.type.model.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class TypeModelFormRelaxedValidator implements RelaxedValidator<TypeModelForm>  {
    @Override
    public Boolean validateLoosely(TypeModelForm form, Errors errors) {
        if(form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeModelForm.name is empty");
            return false;
        }
        log.debug("TypeModelForm.name is valid");
        if(form.getTypeLovId() != null && form.getTypeLovId() <= 0L) {
            errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeModelForm.typeLovId is empty");
            return false;
        }
        log.debug("TypeModelForm.typeLovId is valid");
        return true;
    }
}
