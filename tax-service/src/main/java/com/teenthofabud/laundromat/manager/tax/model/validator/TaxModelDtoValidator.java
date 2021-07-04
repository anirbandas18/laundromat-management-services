package com.teenthofabud.laundromat.manager.tax.model.validator;

import com.teenthofabud.core.common.data.dto.TypeModelDto;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelMessageTemplate;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelDto;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.TaxTypeModelValidator;
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
            try {
                Long taxTypeModelId = Long.parseLong(optTaxTypeModelId.get());
                if(taxTypeModelId <= 0L) {
                    isValid = false;
                    log.debug("TaxModelDto.taxTypeModelId is invalid: taxTypeModelId <= 0");
                } else {
                    Errors internalErrors = new DirectFieldBindingResult(taxTypeModelId, "TaxModelDto.taxTypeModelId");
                    taxTypeModelValidator.validate(taxTypeModelId, internalErrors);
                    if(internalErrors.hasErrors()) {
                        isValid = false;
                        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_TYPE_MODEL_ID_INVALID);
                    }
                }
            } catch (NumberFormatException e) {
                isValid = false;
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_TYPE_MODEL_ID_INVALID);
                log.error(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_TAX_TYPE_MODEL_ID_INVALID, e);
            }
            if(!isValid) {
                errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<TypeModelDto> optCurrencyTypeModel = dto.getCurrencyTypeModel();
        if((optCurrencyTypeModel.isPresent())) {
            boolean isValid = true;
            TypeModelDto currencyTypeModel = optCurrencyTypeModel.get();
            Optional<String> optCurrencyTypeModelId = currencyTypeModel.getId();
            if(optCurrencyTypeModelId.isPresent()) {
                String currencyTypeModelIdExpected = optCurrencyTypeModelId.get();
                try {
                    Long currencyTypeModelIdActual = Long.parseLong(currencyTypeModelIdExpected);
                    if(currencyTypeModelIdActual <= 0L) {
                        isValid = false;
                        log.debug("TaxModelDto.currencyTypeModelId is invalid: currencyTypeModelId <= 0");
                    } else {
                        Errors internalErrors = new DirectFieldBindingResult(currencyTypeModelIdActual, "TaxModelDto.currencyTypeModel.id");
                        currencyTypeModelValidator.validate(currencyTypeModelIdActual, internalErrors);
                        if(internalErrors.hasErrors()) {
                            isValid = false;
                            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INVALID);
                        }
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                    log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INVALID);
                    log.error(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_DTO_CURRENCY_TYPE_MODEL_ID_INVALID, e);
                }

            }
            Optional<String> optCurrencyTypeModelName = currencyTypeModel.getName();
            if(optCurrencyTypeModelName.isPresent() && StringUtils.isEmpty(optCurrencyTypeModelName.get())) {
                isValid = false;
                log.debug("TypeModelDto.currencyTypeModel.name is invalid");
            }
            if(!isValid) {
                errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
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
                return;
            }
        }

        Optional<String> optName = dto.getName();
        if((optName.isPresent()) && StringUtils.isEmpty(optName.get())) {
            log.debug("TaxModelDto.name is invalid");
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                log.debug("TaxModelDto.active is invalid");
                return;
            }
        }
    }
}
