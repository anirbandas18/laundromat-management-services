package com.teenthofabud.laundromat.manager.tax.lov.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TaxLOVForm2EntityMapper implements DualChannelMapper<TaxLOVEntity, TaxLOVForm> {
    @Override
    public Optional<TaxLOVEntity> compareAndMap(TaxLOVEntity actualEntity, TaxLOVForm form) {
        TaxLOVEntity expectedEntity = new TaxLOVEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TaxLOVEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TaxLOVEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TaxLOVEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TaxLOVForm.name: {} is different as TaxLOVEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("TaxLOVForm.name: is unchanged");
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription())) &&
                form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TaxLOVForm.description: {} is different as TaxLOVEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("TaxLOVForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
