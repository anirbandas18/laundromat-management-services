package com.teenthofabud.laundromat.manager.tax.model.validator;

import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.type.validator.TaxTypeModelValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class TaxModelFormValidator implements Validator {

    private TaxTypeModelValidator taxTypeModelValidator;

    @Autowired
    public void setTaxTypeModelValidator(TaxTypeModelValidator taxTypeModelValidator) {
        this.taxTypeModelValidator = taxTypeModelValidator;
    }

    @Autowired
    public void setCurrencyTypeModelValidator(CurrencyTypeModelValidator currencyTypeModelValidator) {
        this.currencyTypeModelValidator = currencyTypeModelValidator;
    }

    private CurrencyTypeModelValidator currencyTypeModelValidator;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TaxModelForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TaxModelForm form = (TaxModelForm) target;
        if(form.getTaxTypeModelId() == null) {
            log.debug("TaxModelForm.taxTypeModelId is invalid");
            errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        } else {
            Errors internalErrors = new DirectFieldBindingResult(form.getTaxTypeModelId(), "TaxModelForm.taxTypeModelId");
            taxTypeModelValidator.validate(form.getTaxTypeModelId(), internalErrors);
            if(internalErrors.hasErrors()) {
                log.debug("TaxModelForm.taxTypeModelId is invalid");
                errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(form.getRate() == null || form.getRate() < 0L) {
            log.debug("TaxModelForm.rate is invalid");
            errors.rejectValue("rate", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
        if(form.getCurrencyTypeModel() == null) {
            log.debug("TaxModelForm.currencyTypeModel is invalid");
            errors.rejectValue("currencyTypeModel", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        } else {
            TaxModelForm.TypeModelForm currencyTypeModelForm = form.getCurrencyTypeModel();
            if(currencyTypeModelForm.getId() == null) {
                log.debug("TaxModelForm.currencyTypeModel.id is invalid");
                errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else {
                Errors internalErrors = new DirectFieldBindingResult(form.getCurrencyTypeModel().getId(), "TaxModelForm.currencyTypeModel.id");
                currencyTypeModelValidator.validate(form.getCurrencyTypeModel().getId(), internalErrors);
                if(internalErrors.hasErrors()) {
                    log.debug("TaxModelForm.currencyTypeModel.id is invalid");
                    errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                    return;
                }
            }
            if(StringUtils.isEmpty(currencyTypeModelForm.getName())) {
                log.debug("TaxModelForm.currencyTypeModel.name is empty");
                errors.rejectValue("currencyTypeModel.name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(StringUtils.isEmpty(form.getName())) {
            log.debug("TaxModelForm.name is empty");
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
