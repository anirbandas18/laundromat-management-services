package com.teenthofabud.laundromat.manager.tax.model.mapper;

import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TaxModelEntitySelfMapper implements SingleChannelMapper<TaxModelEntity> {
    @Override
    public Optional<TaxModelEntity> compareAndMap(TaxModelEntity source, TaxModelEntity target) {
        boolean changeSW = false;
         if(source.getId() != null && source.getId() > 0 && !source.getId().equals(target.getId())) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TaxModelEntity.id is valid");
        }
        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TaxModelEntity.description is valid");
        }
        if(source.getName() != null && StringUtils.hasText(source.getName()) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source TaxModelEntity.name is valid");
        }
        if(source.getRate() != null && source.getRate() > 0 && !source.getRate().equals(target.getRate())) {
            target.setRate(source.getRate());
            changeSW = true;
            log.debug("Source TaxModelEntity.rate is valid");
        }
        if(source.getTaxTypeModelId() != null && source.getTaxTypeModelId() > 0
                && !source.getTaxTypeModelId().equals(target.getTaxTypeModelId())) {
            target.setTaxTypeModelId(source.getTaxTypeModelId());
            changeSW = true;
            log.debug("Source TaxModelEntity.taxTypeModelId is valid");
        }
        if(source.getCurrencyTypeModel() != null && source.getCurrencyTypeModel().getId() != null && source.getCurrencyTypeModel().getId() > 0
                && !source.getCurrencyTypeModel().getId().equals(target.getCurrencyTypeModel().getId())) {
            if(target.getCurrencyTypeModel() == null) {
                target.setCurrencyTypeModel(new TypeModelEntity());
            }
            target.getCurrencyTypeModel().setId(source.getCurrencyTypeModel().getId());
            changeSW = true;
            log.debug("Source TaxModelEntity.currencyTypeModelId is valid");
        }
        if(source.getCurrencyTypeModel() != null && StringUtils.hasText(source.getCurrencyTypeModel().getName())
                && source.getCurrencyTypeModel().getName().compareTo(target.getCurrencyTypeModel().getName()) != 0) {
            if(target.getCurrencyTypeModel() == null) {
                target.setCurrencyTypeModel(new TypeModelEntity());
            }
            target.getCurrencyTypeModel().setName(source.getCurrencyTypeModel().getName());
            changeSW = true;
            log.debug("Source TaxModelEntity.currencyName is valid");
        }
        if(changeSW) {
            log.debug("All provided TaxModelEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TaxModelEntity attributes are valid");
            return Optional.empty();
        }
    }
}
