package com.teenthofabud.laundromat.manager.tax.model.validator;

import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelDto;
import com.teenthofabud.laundromat.manager.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.type.validator.TaxTypeModelValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class TaxModelDtoValidator implements Validator {

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
        return clazz.isAssignableFrom(TaxModelDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TaxModelDto dto = (TaxModelDto) target;

        Optional<String> optTaxTypeModelId = dto.getTaxTypeModelId();
        if((optTaxTypeModelId.isPresent())) {
            boolean isValid = true;
            if(optTaxTypeModelId.get() == null) {
                isValid = false;
                log.debug("TaxModelDto.taxTypeModelId is null");
            } else {
                try {
                    Long taxTypeModelId = Long.parseLong(optTaxTypeModelId.get());
                    if(taxTypeModelId <= 0L) {
                        isValid = false;
                        log.debug("TaxModelDto.taxTypeModelId is invalid: taxTypeModelId <= 0");
                    } else {
                        Errors internalErrors = new DirectFieldBindingResult(taxTypeModelId, "TaxModelDto.taxTypeModelId");
                        taxTypeModelValidator.validate(taxTypeModelId, internalErrors);
                        if(internalErrors.hasErrors()) {
                            log.debug("TaxModelDto.taxTypeModelId is invalid");
                            errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                            return;
                        }
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                    log.debug("TaxModelDto.taxTypeModelId is invalid");
                    log.error("TaxModelDto.taxTypeModelId is invalid", e);
                }
            }
            if(!isValid) {
                errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            }
            return;
        }

        Optional<String> optCurrencyTypeModelId = dto.getCurrencyTypeModelId();
        if((optCurrencyTypeModelId.isPresent())) {
            boolean isValid = true;
            if(optCurrencyTypeModelId.get() == null) {
                isValid = false;
                log.debug("TaxModelDto.taxTypeModelId is null");
            } else {
                try {
                    Long currencyTypeModelId = Long.parseLong(optCurrencyTypeModelId.get());
                    if(currencyTypeModelId <= 0L) {
                        isValid = false;
                        log.debug("TaxModelDto.taxTypeModelId is invalid: taxTypeModelId <= 0");
                    } else {
                        Errors internalErrors = new DirectFieldBindingResult(currencyTypeModelId, "TaxModelDto.currencyTypeModelForm.id");
                        currencyTypeModelValidator.validate(currencyTypeModelId, internalErrors);
                        if(internalErrors.hasErrors()) {
                            log.debug("TaxModelDto.currencyTypeModelForm.id is invalid");
                            errors.rejectValue("currencyTypeModelForm.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                            return;
                        }
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                    log.debug("TaxModelDto.currencyTypeModel.id is invalid");
                    log.error("TaxModelDto.currencyTypeModel.id is invalid", e);
                }
            }
            if(!isValid) {
                errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            }
            return;
        }

        Optional<String> optRate = dto.getRate();
        if((optRate.isPresent())) {
            boolean isValid = true;
            if(optRate.get() == null) {
                isValid = false;
                log.debug("TaxModelDto.rate is null");
            } else {
                try {
                    Float rate = Float.parseFloat(optRate.get());
                    if(rate < 0L) {
                        isValid = false;
                        log.debug("TaxModelDto.rate is invalid: taxTypeModelId <= 0");
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                    log.debug("TaxModelDto.rate is invalid");
                    log.error("TaxModelDto.rate is invalid", e);
                }
            }
            if(!isValid) {
                errors.rejectValue("rate", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            }
            return;
        }

        Optional<String> optCurrencyTypeModelName = dto.getCurrencyTypeModelName();
        if((optCurrencyTypeModelName.isPresent()) && StringUtils.isEmpty(optCurrencyTypeModelName.get())) {
            log.debug("TaxModelForm.currencyTypeModelName is invalid");
            errors.rejectValue("currencyTypeModelName", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }

        Optional<String> optName = dto.getName();
        if((optName.isPresent()) && StringUtils.isEmpty(optName.get())) {
            log.debug("TaxModelForm.name is invalid");
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }

        Optional<String> optActive = dto.getActive();
        if((optActive.isPresent()) && StringUtils.isEmpty(optActive.get())
            && ((optActive.get().compareToIgnoreCase(Boolean.TRUE.toString()) != 0) || (optActive.get().compareToIgnoreCase(Boolean.FALSE.toString()) != 0))) {
            log.debug("TaxModelForm.active is invalid");
            errors.rejectValue("active", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
