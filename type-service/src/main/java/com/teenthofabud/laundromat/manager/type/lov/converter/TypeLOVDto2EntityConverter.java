package com.teenthofabud.laundromat.manager.type.lov.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVDto;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;

@Component
@Slf4j
public class TypeLOVDto2EntityConverter implements ComparativePatchConverter<TypeLOVDto, TypeLOVEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    @Autowired
    public void setTypeLOVRepository(TypeLOVRepository typeLOVRepository) {
        this.typeLOVRepository = typeLOVRepository;
    }

    private TypeLOVRepository typeLOVRepository;


    @Override
    public void compareAndMap(TypeLOVDto dto, TypeLOVEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        if(dto.getDescription().isPresent()) {
            actualEntity.setDescription(dto.getDescription().get());
            changeSW[i++] = true;
            log.debug("TypeLOVDto.description is valid");
        }
        if(dto.getName().isPresent()) {
            actualEntity.setName(dto.getName().get());
            changeSW[i++] = true;
            log.debug("TypeLOVDto.name is valid");
        }
        if(dto.getActive().isPresent()) {
            actualEntity.setActive(Boolean.valueOf(dto.getActive().get()));
            changeSW[i++] = true;
            log.debug("TypeLOVDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TypeLOVDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TypeLOVDto attributes are valid");
    }
}
