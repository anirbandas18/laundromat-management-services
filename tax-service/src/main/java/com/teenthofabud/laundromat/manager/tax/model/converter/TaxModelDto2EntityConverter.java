package com.teenthofabud.laundromat.manager.tax.model.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelDto;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class TaxModelDto2EntityConverter implements ComparativePatchConverter<TaxModelDto, TaxModelEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 7;

    @Override
    public void compareAndMap(TaxModelDto dto, TaxModelEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optDescription = dto.getDescription();
        if(optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.description is valid");
        }
        Optional<String> optName = dto.getName();
        if(optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.name is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.active is valid");
        }
        Optional<String> optRate = dto.getRate();
        if(optRate.isPresent()) {
            actualEntity.setRate(Float.parseFloat(optRate.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.rate is valid");
        }
        Optional<String> optTaxTypeModelId = dto.getTaxTypeModelId();
        if(optTaxTypeModelId.isPresent()) {
            actualEntity.setTaxTypeModelId(Long.parseLong(optTaxTypeModelId.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.taxTypeModelId is valid");
        }
        Optional<String> optCurrencyTypeModelId = dto.getCurrencyTypeModelId();
        if(optCurrencyTypeModelId.isPresent()) {
            actualEntity.setCurrencyTypeModelId(Long.parseLong(optCurrencyTypeModelId.get()));
            changeSW[i++] = true;
            log.debug("TaxModelDto.currencyTypeModelId is valid");
        }
        Optional<String> optCurrencyTypeModelName = dto.getCurrencyTypeModelName();
        if(optCurrencyTypeModelName.isPresent()) {
            actualEntity.setCurrencyName(optCurrencyTypeModelName.get());
            changeSW[i++] = true;
            log.debug("TaxModelDto.currencyTypeModelName is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TaxModelDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TaxModelDto attributes are valid");
    }
}
