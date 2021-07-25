package com.teenthofabud.laundromat.manager.tax.lov.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TaxLOVEntitySelfMapper implements SingleChannelMapper<TaxLOVEntity> {
    @Override
    public Optional<TaxLOVEntity> compareAndMap(TaxLOVEntity source, TaxLOVEntity target) {
        TaxLOVEntity expectedEntity = new TaxLOVEntity();
        boolean changeSW = false;
        if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TaxLOVEntity.id is valid");
        }
        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TaxLOVEntity.description is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source TaxLOVEntity.name is valid");
        }
        if(changeSW) {
            log.debug("All provided TaxLOVEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TaxLOVEntity attributes are valid");
            return Optional.empty();
        }
    }
}
