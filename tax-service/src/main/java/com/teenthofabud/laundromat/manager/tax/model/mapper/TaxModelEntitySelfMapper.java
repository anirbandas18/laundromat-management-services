package com.teenthofabud.laundromat.manager.tax.model.mapper;

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
        TaxModelEntity expectedEntity = new TaxModelEntity();
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
        if(source.getCurrencyTypeModelId() != null && source.getCurrencyTypeModelId() > 0
                && !source.getCurrencyTypeModelId().equals(target.getCurrencyTypeModelId())) {
            target.setCurrencyTypeModelId(source.getCurrencyTypeModelId());
            changeSW = true;
            log.debug("Source TaxModelEntity.currencyTypeModelId is valid");
        }
        if(source.getCurrencyName() != null && StringUtils.hasText(source.getCurrencyName())
                && source.getCurrencyName().compareTo(target.getCurrencyName()) != 0) {
            target.setCurrencyName(source.getCurrencyName());
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
