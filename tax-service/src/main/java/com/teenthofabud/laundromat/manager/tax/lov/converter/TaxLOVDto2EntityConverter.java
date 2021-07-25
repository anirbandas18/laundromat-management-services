package com.teenthofabud.laundromat.manager.tax.lov.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVDto;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class TaxLOVDto2EntityConverter implements ComparativePatchConverter<TaxLOVDto, TaxLOVEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;


    @Override
    public void compareAndMap(TaxLOVDto dto, TaxLOVEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optDescription = dto.getDescription();
        if(optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("TaxLOVDto.description is valid");
        }
        Optional<String> optName = dto.getName();
        if(optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("TaxLOVDto.name is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TaxLOVDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TaxLOVDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TaxLOVDto attributes are valid");
    }
}
