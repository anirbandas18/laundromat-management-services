package com.teenthofabud.laundromat.manager.tax.lov.validator;

import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TaxLOVFormValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TaxLOVForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TaxLOVForm form = (TaxLOVForm) target;
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("TaxLOVForm.name is empty");
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
