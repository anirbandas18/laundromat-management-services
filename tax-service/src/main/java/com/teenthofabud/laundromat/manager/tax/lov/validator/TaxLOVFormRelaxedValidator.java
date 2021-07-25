package com.teenthofabud.laundromat.manager.tax.lov.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class TaxLOVFormRelaxedValidator implements RelaxedValidator<TaxLOVForm>  {
    @Override
    public Boolean validateLoosely(TaxLOVForm form, Errors errors) {
        if(form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxLOVForm.name is empty");
            return false;
        }
        log.debug("TaxLOVForm.name is valid");
        return true;
    }
}
