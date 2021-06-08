package com.teenthofabud.laundromat.manager.tax.model.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.core.common.data.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.tax.constant.TaxSubDomain;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.proxy.TypeServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TaxModelForm2EntityMapper implements DualChannelMapper<TaxModelEntity, TaxModelForm> {

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
            //changeSW = true;
            log.debug("TaxModelForm.name: is unchanged");
        }
        if(StringUtils.hasText(form.getDescription()) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TaxModelForm.description: {} is different as TaxModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            //changeSW = true;
            log.debug("TaxModelForm.description: is unchanged");
        }
        if(form.getRate() != null && !form.getRate().equals(actualEntity.getRate())) {
            expectedEntity.setRate(form.getRate());
            changeSW = true;
            log.debug("TaxModelForm.rate: {} is different as TaxModelEntity.rate: {}", form.getRate(), actualEntity.getRate());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            //changeSW = true;
            log.debug("TaxModelForm.rate: is unchanged");
        }
        if(form.getTaxTypeModelId() != null &&
                !form.getTaxTypeModelId().equals(actualEntity.getTaxTypeModelId())) {
            try {
                TypeModelVo taxTypeModelVo = typeServiceClient.getTypeModelDetailsById(form.getTaxTypeModelId());
                if(taxTypeModelVo == null || taxTypeModelVo.getId() == null || taxTypeModelVo.getId() != form.getTaxTypeModelId()
                    || taxTypeModelVo.getTypeLovVo() == null || taxTypeModelVo.getTypeLovVo().getId() == null
                        || taxTypeModelVo.getTypeLovVo().getId() != taxTypeLovId) {
                    throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_NOT_FOUND,
                            new Object [] { "taxTypeModelId", String.valueOf(form.getTaxTypeModelId()) });
                } else {
                    expectedEntity.setTaxTypeModelId(form.getTaxTypeModelId());
                    changeSW = true;
                    log.debug("TaxModelForm.taxTypeModelId: {} is different as TaxModelEntity.taxTypeModelId: {}",
                            form.getTaxTypeModelId(), actualEntity.getTaxTypeModelId());
                }
            } catch (TypeException e) {
                log.debug("TaxModelForm.taxTypeModelId is invalid");
                log.error("TaxModelForm.taxTypeModelId is invalid", e);
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_NOT_FOUND,
                        new Object [] { "taxTypeModelId", String.valueOf(form.getTaxTypeModelId()) });
            }
        } else {
            expectedEntity.setTaxTypeModelId(actualEntity.getTaxTypeModelId());
            //changeSW = true;
            log.debug("TaxModelForm.taxTypeModelId: is unchanged");
        }
        if(form.getCurrencyTypeModelForm() != null &&
                !form.getCurrencyTypeModelForm().getId().equals(actualEntity.getCurrencyTypeModelId())) {
            try {
                TypeModelVo currencyTypeModelVo = typeServiceClient.getTypeModelDetailsById(form.getCurrencyTypeModelForm().getId());
                if(currencyTypeModelVo == null || currencyTypeModelVo.getId() == null || currencyTypeModelVo.getId() != form.getCurrencyTypeModelForm().getId()
                        || currencyTypeModelVo.getTypeLovVo() == null || currencyTypeModelVo.getTypeLovVo().getId() == null
                        || currencyTypeModelVo.getTypeLovVo().getId() != currencyTypeLovId) {
                    throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_NOT_FOUND,
                            new Object [] { "currencyTypeLov.id", String.valueOf(form.getCurrencyTypeModelForm().getId()) });
                } else {
                    expectedEntity.setCurrencyTypeModelId(form.getCurrencyTypeModelForm().getId());
                    changeSW = true;
                    log.debug("TaxModelForm.currencyTypeLov.id: {} is different as TaxModelEntity.currencyTypeModelId: {}",
                            form.getCurrencyTypeModelForm().getId(), actualEntity.getCurrencyName());
                }
            } catch (TypeException e) {
                log.debug("TaxModelForm.currencyTypeLov.id is invalid");
                log.error("TaxModelForm.currencyTypeLov.id is invalid", e);
                throw new TaxException(TaxSubDomain.TAX_MODEL, TaxErrorCode.TAX_NOT_FOUND,
                        new Object [] { "currencyTypeLov.id", String.valueOf(form.getCurrencyTypeModelForm().getId()) });
            }
        } else {
            expectedEntity.setCurrencyTypeModelId(actualEntity.getCurrencyTypeModelId());
            //changeSW = true;
            log.debug("TaxModelForm.currencyTypeLov.id: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
