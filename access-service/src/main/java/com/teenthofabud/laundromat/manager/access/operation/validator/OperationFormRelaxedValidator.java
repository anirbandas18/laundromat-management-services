package com.teenthofabud.laundromat.manager.access.operation.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class OperationFormRelaxedValidator implements RelaxedValidator<OperationForm>  {
    @Override
    public Boolean validateLoosely(OperationForm form, Errors errors) {
        if(form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("OperationForm.name is empty");
            return false;
        }
        log.debug("OperationForm.name is valid");
        return true;
    }
}
