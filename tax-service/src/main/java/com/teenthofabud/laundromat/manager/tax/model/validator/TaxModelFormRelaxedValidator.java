package com.teenthofabud.laundromat.manager.tax.model.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

@Component
@Slf4j
public class TaxModelFormRelaxedValidator implements RelaxedValidator<TaxModelForm>  {
    @Override
    public Boolean validateLoosely(TaxModelForm form, Errors errors) {
        if(form.getName() != null && form.getName().length() == 0) {
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxModelForm.name is empty");
            return false;
        }
        log.debug("TaxModelForm.name is valid");
        if(form.getRate() != null && form.getRate() <= 0L) {
            errors.rejectValue("rate", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxModelForm.rate is empty");
            return false;
        }
        log.debug("TaxModelForm.rate is valid");
        if(form.getTaxTypeModelId() != null && form.getTaxTypeModelId() <= 0L) {
            errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxModelForm.taxTypeModelId is empty");
            return false;
        }
        log.debug("TaxModelForm.taxTypeModelId is valid");
        if(form.getCurrencyTypeModel() != null &&
                (form.getCurrencyTypeModel().getId() <= 0L || StringUtils.isEmpty(form.getCurrencyTypeModel().getName()))) {
            errors.rejectValue("currencyTypeModel", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxModelForm.currencyTypeModel is empty");
            return false;
        }
        log.debug("TaxModelForm.currencyTypeModel is valid");
        return true;
    }
}
