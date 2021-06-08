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
public class TaxTypeModelValidator implements Validator {

    @Autowired
    public void setTypeServiceClient(TypeServiceClient typeServiceClient) {
        this.typeServiceClient = typeServiceClient;
    }

    private TypeServiceClient typeServiceClient;

    @Value("${lms.tax.taxtypelov.id}")
    public void setTaxTypeLovId(Long taxTypeLovId) {
        this.taxTypeLovId = taxTypeLovId;
    }

    private Long taxTypeLovId;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(Long.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        Long taxTypeModelId = (Long) target;

        try {
            TypeModelVo taxTypeModelVo = typeServiceClient.getTypeModelDetailsById(taxTypeModelId);
            if(taxTypeModelVo.getId() == null) {
                log.debug("taxTypeModelId is null");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (!taxTypeModelVo.getId().equals(taxTypeModelId)) {
                log.debug("taxTypeModelId doesn't match with registered value");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (taxTypeModelVo.getTypeLovVo() == null || taxTypeModelVo.getTypeLovVo().getId() == null) {
                log.debug("taxTypeModel.typeLovId is invalid");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            } else if (!taxTypeModelVo.getTypeLovVo().getId().equals(taxTypeLovId)) {
                log.debug("taxTypeModel.typeLovId is not for any registered tax type models");
                errors.reject(TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                return;
            }
        } catch (TypeException e) {
            log.debug("taxTypeModelId is invalid");
            log.error("taxTypeModelId is invalid", e);
            errors.rejectValue("taxTypeModelId", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
