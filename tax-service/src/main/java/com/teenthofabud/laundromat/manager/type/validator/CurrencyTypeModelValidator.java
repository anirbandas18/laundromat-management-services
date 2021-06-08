package com.teenthofabud.laundromat.manager.type.validator;

import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.proxy.TypeServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class CurrencyTypeModelValidator implements Validator {
    @Autowired
    public void setTypeServiceClient(TypeServiceClient typeServiceClient) {
        this.typeServiceClient = typeServiceClient;
    }

    private TypeServiceClient typeServiceClient;

    @Value("${lms.tax.currencytypelov.id}")
    public void setCurrencyTypeLovId(Long currencyTypeLovId) {
        this.currencyTypeLovId = currencyTypeLovId;
    }

    private Long currencyTypeLovId;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(Long.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        Long currencyTypeModelId = (Long) target;
        try {
            TypeModelVo currencyTypeModelVo = typeServiceClient.getTypeModelDetailsById(currencyTypeModelId);
            if(currencyTypeModelVo.getId() == null) {
                log.debug("currencyTypeModel.id is null");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                //errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (currencyTypeModelVo.getId() != currencyTypeModelId) {
                log.debug("currencyTypeModel.id doesn't match with registered value");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                //errors.rejectValue("currencyTypeModel.id", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (currencyTypeModelVo.getTypeLovVo() == null || currencyTypeModelVo.getTypeLovVo().getId() == null) {
                log.debug("currencyTypeModel.typeLovId is invalid");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                //errors.rejectValue("currencyTypeModel.typeLovId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (currencyTypeModelVo.getTypeLovVo().getId() != currencyTypeLovId) {
                log.debug("currencyTypeModel.typeLovId is not for any registered currency type models");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                //errors.rejectValue("currencyTypeModel.typeLovId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
        } catch (TypeException e) {
            log.debug("currencyTypeModelId is invalid");
            log.error("currencyTypeModelId is invalid", e);
            errors.rejectValue("currencyTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
