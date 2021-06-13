package com.teenthofabud.laundromat.manager.tax.model.mapper;

import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.tax.constant.TaxSubDomain;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.TaxTypeModelValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.Optional;

@Component
@Slf4j
public class TaxModelForm2EntityMapper implements DualChannelMapper<TaxModelEntity, TaxModelForm> {


    @Autowired
    public void setTaxTypeModelValidator(TaxTypeModelValidator taxTypeModelValidator) {
        this.taxTypeModelValidator = taxTypeModelValidator;
    }

    private TaxTypeModelValidator taxTypeModelValidator;

    @Autowired
    public void setCurrencyTypeModelValidator(CurrencyTypeModelValidator currencyTypeModelValidator) {
        this.currencyTypeModelValidator = currencyTypeModelValidator;
    }

    private CurrencyTypeModelValidator currencyTypeModelValidator;

    @Value("${lms.tax.taxtypelov.id}")
    public void setTaxTypeLovId(Long taxTypeLovId) {
        this.taxTypeLovId = taxTypeLovId;
    }

    private Long taxTypeLovId;

    @Value("${lms.tax.currencytypelov.id}")
    public void setCurrencyTypeLovId(Long currencyTypeLovId) {
        this.currencyTypeLovId = currencyTypeLovId;
    }

    private Long currencyTypeLovId;

    @Override
    public Optional<TaxModelEntity> compareAndMap(TaxModelEntity actualEntity, TaxModelForm form) throws TOABBaseException {
        TaxModelEntity expectedEntity = new TaxModelEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TaxModelEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TaxModelEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TaxModelEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(form.getName()) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TaxModelForm.name: {} is different as TaxModelEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("TaxModelForm.name: is unchanged");
        }
        if(StringUtils.hasText(form.getDescription()) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TaxModelForm.description: {} is different as TaxModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("TaxModelForm.description: is unchanged");
        }
        if(form.getRate() != null && !form.getRate().equals(actualEntity.getRate())) {
            expectedEntity.setRate(form.getRate());
            changeSW = true;
            log.debug("TaxModelForm.rate: {} is different as TaxModelEntity.rate: {}", form.getRate(), actualEntity.getRate());
        } else {
            expectedEntity.setRate(actualEntity.getRate());
            log.debug("TaxModelForm.rate: is unchanged");
        }
        if(form.getTaxTypeModelId() != null &&
                !form.getTaxTypeModelId().equals(actualEntity.getTaxTypeModelId())) {
            Errors internalErrors = new DirectFieldBindingResult(form.getTaxTypeModelId(), "TaxModelForm.taxTypeModelId");
            taxTypeModelValidator.validate(form.getTaxTypeModelId(), internalErrors);
            if(internalErrors.hasErrors()) {
                log.debug("TaxModelForm.taxTypeModelId is invalid");
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_NOT_FOUND,
                        new Object [] { "taxTypeModelId", String.valueOf(form.getTaxTypeModelId()) });
            } else {
                expectedEntity.setTaxTypeModelId(form.getTaxTypeModelId());
                changeSW = true;
                log.debug("TaxModelForm.taxTypeModelId: {} is different as TaxModelEntity.taxTypeModelId: {}",
                        form.getTaxTypeModelId(), actualEntity.getTaxTypeModelId());
            }
        } else {
            expectedEntity.setTaxTypeModelId(actualEntity.getTaxTypeModelId());
            log.debug("TaxModelForm.taxTypeModelId: is unchanged");
        }
        if(expectedEntity.getCurrencyTypeModel() == null) {
            expectedEntity.setCurrencyTypeModel(new TypeModelEntity());
        }
        if(form.getCurrencyTypeModel() != null && StringUtils.hasText(form.getCurrencyTypeModel().getName())
                && !form.getCurrencyTypeModel().getName().equalsIgnoreCase(actualEntity.getCurrencyTypeModel().getName())) {
            expectedEntity.getCurrencyTypeModel().setName(form.getCurrencyTypeModel().getName());
            changeSW = true;
            log.debug("TaxModelForm.currencyTypeModel.name: {} is different as TaxModelEntity.currencyTypeModel.name: {}",
                    form.getCurrencyTypeModel().getName(), actualEntity.getCurrencyTypeModel().getName());
        } else {
            expectedEntity.getCurrencyTypeModel().setName(actualEntity.getCurrencyTypeModel().getName());
            log.debug("TaxModelForm.currencyTypeModel.name: is unchanged");
        }
        if(form.getCurrencyTypeModel() != null &&
                !form.getCurrencyTypeModel().getId().equals(actualEntity.getCurrencyTypeModel().getId())) {
            Errors internalErrors = new DirectFieldBindingResult(form.getCurrencyTypeModel().getId(), "TaxModelForm.currencyTypeModel.id");
            currencyTypeModelValidator.validate(form.getCurrencyTypeModel().getId(), internalErrors);
            if(internalErrors.hasErrors()) {
                log.debug("TaxModelForm.currencyTypeModel.id is invalid");
                throw new TaxException(TaxSubDomain.MODEL, TaxErrorCode.TAX_NOT_FOUND,
                        new Object [] { "currencyTypeModel.id", String.valueOf(form.getCurrencyTypeModel().getId()) });
            } else {
                expectedEntity.getCurrencyTypeModel().setId(form.getCurrencyTypeModel().getId());
                changeSW = true;
                log.debug("TaxModelForm.currencyTypeModel.id: {} is different as TaxModelEntity.currencyTypeModel.id: {}",
                        form.getCurrencyTypeModel().getId(), actualEntity.getCurrencyTypeModel().getId());
            }
        } else {
            expectedEntity.getCurrencyTypeModel().setId(actualEntity.getCurrencyTypeModel().getId());
            log.debug("TaxModelForm.currencyTypeModel.id: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
